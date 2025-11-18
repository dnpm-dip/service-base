package de.dnpm.dip.service.query


import java.time.{
  Instant,
  LocalDateTime
}
import scala.util.{
  Either,
  Left,
  Right
}
import cats.Monad
import cats.data.EitherNel
import de.dnpm.dip.util.{
  Logging,
  Completer
}
import de.dnpm.dip.coding.Coding
import de.dnpm.dip.model.{
  Id,
  Patient,
  Snapshot,
  Site
}
import de.dnpm.dip.service.{
  Connector,
  ConnectionStatus,
}
import play.api.libs.json.{
  Json,
  Format,
}


abstract class BaseQueryService[
  F[+_],
  UseCase <: UseCaseConfig,
](
  implicit 
  fpr: Format[UseCase#PatientRecord],
  fcrit: Format[UseCase#Criteria]
)
extends QueryService[
  F,Monad[F],UseCase
]
with Logging
{

  import scala.util.chaining._
  import cats.syntax.either._
  import cats.syntax.ior._
  import cats.syntax.applicative._
  import cats.syntax.functor._
  import cats.syntax.flatMap._
  import de.dnpm.dip.util.Completer.syntax._

  
  protected val preparedQueryDB: PreparedQueryDB[F,Monad[F],Criteria,String]
  protected val db: LocalDB[F,Monad[F],Criteria,PatientRecord]
  protected val connector: Connector[F,Monad[F]]
  protected val cache: QueryCache[Criteria,Results,PatientRecord] 


  protected implicit val criteriaCompleter: Completer[Criteria]

  // Completer[Criteria] to allow expanding the criteria,
  // e.g. including sub-classes of concepts, etc
  // separately from the completed criteria returned to the client
  protected val CriteriaExpander: Completer[Criteria]



  protected def ResultSetFrom(
    query: Query[Criteria],
    results: Seq[Query.Match[PatientRecord,Criteria]]
  ): Results
 

  protected implicit val siteCompleter: Completer[Coding[Site]] = {

    val sites =
      Site.local :: connector.otherSites.toList

    Completer.of(
      site =>
        sites.find(_.code == site.code).getOrElse(site)
    )
  }


  override def statusInfo(
    implicit env: Monad[F]
  ): F[QueryService.StatusInfo] =
    db.totalRecords
      .map(QueryService.StatusInfo(_))


  override def sites(
    implicit
    env: Monad[F]
  ): F[Sites] =
    Sites(
      Site.local,
      connector.otherSites.toList
    )
    .pure

  override def !(
    cmd: PreparedQuery.Command[Criteria]
  )(
    implicit 
    env: Monad[F],
    querier: Querier
  ): F[String EitherNel PreparedQuery[Criteria]] = {

    import de.dnpm.dip.util.Operations.syntax._
    import PreparedQuery.{Create,Update,Delete}

    cmd match {

      case Create(name,criteria) =>
        log.info(s"Processing new PreparedQuery by $querier")

        for { 
          id <-
            preparedQueryDB.newId

          pq =
            PreparedQuery(
              id,
              querier,
              name,
              criteria.complete,
              LocalDateTime.now,
              Instant.now
            )

          result <-
            preparedQueryDB.save(pq)
              .map(_ => pq.asRight[String])

        } yield result.toEitherNel


      case Update(id,optName,optCriteria) =>

        log.info(s"Updating PreparedQuery $id by $querier")

        for {
          optPq <-
            preparedQueryDB.get(id)

          optUpdated =
            optPq.map(
              _.patch(
                optName.map(name => _.copy(name = name)),
                optCriteria.map(crit => _.copy(criteria = crit.complete)),
              )
              .update(
                _.copy(lastUpdate = Instant.now)
              )
            )

          result <-
            optUpdated match {
              case Some(updated) =>
                preparedQueryDB.save(updated)
                  .map(_ => updated.asRight[String])

              case None => 
                s"Invalid PreparedQuery ID $id"
                  .asLeft[PreparedQuery[Criteria]]
                  .pure
            }

        } yield result.toEitherNel 


      case Delete(id) =>

        log.info(s"Deleting PreparedQuery $id by $querier")

        preparedQueryDB
          .delete(id)
          .map(
            _.toRight(s"Invalid PreparedQuery ID $id")
             .toEitherNel
          )

    }

  }


  override def ?(
    id: PreparedQuery.Id
  )(
    implicit 
    env: Monad[F],
    querier: Querier
  ): F[Option[PreparedQuery[Criteria]]] = {

    log.info(s"Retrieving PreparedQuery $id for $querier")

    preparedQueryDB.get(id)

  }


  override def ?(
    filter: PreparedQuery.Filter
  )(
    implicit
    env: Monad[F],
    querier: Querier
  ): F[Seq[PreparedQuery[Criteria]]] = {

    log.info(s"Retrieving PreparedQueries for $querier")
    
    preparedQueryDB.query(filter)
  }


  import QueryService._

  override def !(
    cmd: DataCommand[PatientRecord]
  )(
    implicit 
    env: Monad[F]
  ): F[Either[DataError,DataOutcome[PatientRecord]]] = {

    cmd match {

      case Save(dataSet) =>
        log.info(s"Saving new patient record")
        db.save(dataSet)
          .map(_.leftMap(GenericError(_)))

      case Delete(patient) =>
        log.info(s"Deleting all data for Patient $patient")
        db.delete(patient)
          .map(_.leftMap(GenericError(_)))

    }
  }


  import Query.Mode.{Local,Federated,Custom}

  override def !(
    cmd: Query.Command[Criteria]
  )(
    implicit
    env: Monad[F],
    querier: Querier
  ): F[Either[Query.Error,Query[Criteria]]] = {

    def modeAndSites(
      mode: Coding[Query.Mode.Value],
      sites: Option[Set[Coding[Site]]]
    ): (Coding[Query.Mode.Value],Set[Coding[Site]]) =
      mode match {
        case Query.Mode(Local) => 
          Coding(Local) -> Set(Site.local)

        case Query.Mode(Custom) => 
          //TODO: consider changing to return an error if site list is undefined on "custom" query
          Coding(Custom) -> sites.getOrElse(connector.otherSites + Site.local)

        case _ => 
          Coding(Federated) -> (connector.otherSites + Site.local)
      }


    cmd match {

      case submit @ Query.Submit(optMode,optSites,crit) => {

        log.info(s"Processing new query by $querier: \n${Json.prettyPrint(Json.toJson(submit))}") 

        val id = cache.newQueryId

        val (mode,sites) = modeAndSites(optMode,optSites.complete)

        //TODO: criteria validation
        val criteria = crit.complete

        for {
          resultsBySite <- executeQuery(id,sites,criteria) 

          errsOrResults =
            resultsBySite
              .values
              .map(_.toIor.toIorNel)
              .reduceOption(_ combine _)
              .getOrElse(Seq.empty.rightIor)

          errsOrQuery = errsOrResults.toEither match {
            case Right(results) if (results.nonEmpty) =>
              Query[Criteria](
                id,
                LocalDateTime.now,
                querier,
                mode,
                ConnectionStatus.from(resultsBySite),
                criteria,
                cache.timeoutSeconds,
                Instant.now
              )
              .tap(query => cache += query -> ResultSetFrom(query,results))
              .asRight

            case Right(_) => Query.NoResults.asLeft
                
            case Left(errs) => Query.ConnectionErrors(errs).asLeft
          }

        } yield errsOrQuery

      }

      case update @ Query.Update(id,optMode,optSites,optCriteria) => {

        log.info(s"Updating Query $id by $querier: \n${Json.prettyPrint(Json.toJson(update))}") 
        
        cache.getQuery(id) match {

          case None => Query.InvalidId.asLeft.pure[F]

          case Some(query) => {

            val (mode,sites) = modeAndSites(optMode.getOrElse(query.mode),optSites.complete)

            val sitesChanged = sites != query.peers.map(_.site).toSet

            //TODO: criteria validation
              
            val criteria = optCriteria.complete

            val criteriaChanged =
              (criteria,query.criteria) match {
                case (Some(n),Some(prev)) if n == prev => false
                case _ => true
              }


            if (sitesChanged || criteriaChanged){

              log.debug(s"Query target sites or criteria changed, re-submitting...") 

              for {
                resultsBySite <- executeQuery(id,sites,criteria) 
              
                errsOrResults =
                  resultsBySite
                    .values
                    .map(_.toIor.toIorNel)
                    .reduceOption(_ combine _)
                    .getOrElse(Seq.empty.rightIor)

                errsOrQuery = errsOrResults.toEither match {
                  case Right(results) if (results.nonEmpty) =>
                    Query[Criteria](
                      id,
                      LocalDateTime.now,
                      querier,
                      mode,
                      ConnectionStatus.from(resultsBySite),
                      criteria,
                      cache.timeoutSeconds,
                      Instant.now
                    )
                    .tap(query => cache += query -> ResultSetFrom(query,results))
                    .asRight
      
                  case Right(_) => Query.NoResults.asLeft

                  case Left(errs) => Query.ConnectionErrors(errs).asLeft
                }
              
              } yield errsOrQuery
              
            } else {
              log.debug(s"Query target sites or criteria unchanged, nothing to do") 
              query.asRight.pure[F]
            }

          }
        }
      }

      case Query.Delete(id) => {

        log.info(s"Deleting Query $id by $querier") 

        cache.getQuery(id) match {

          case None => Query.InvalidId.asLeft.pure[F]

          case Some(query) => 
            cache -= id
            query.asRight.pure[F]

        }

      }

    }

  }


  // Introduced as a (temporary) workaround:
  // The current data protection concept doesn't allow federated queries.
  // To avoid changing all components from the UI to here, just execute any query locally under the hood.
  private def executeQuery(
    id: Query.Id,
    sites: Set[Coding[Site]],
    criteria: Option[Criteria]
  )(
    implicit env: Monad[F]
  ): F[Map[Coding[Site],Either[String,Seq[Query.Match[PatientRecord,Criteria]]]]] = {

    // Expand the query criteria only here,
    // to save bandwidth transmitting them to peers and
    // to avoid "log pollution" with potentially very long expanded criteria 
    sites.contains(Site.local) match {
      case true =>
        (db ? criteria.map(CriteriaExpander))
          .map(results => Map(Site.local -> results))

      case _ => Map.empty.pure[F]
    }

  }

/*
  private def executeQuery(
    id: Query.Id,
    sites: Set[Coding[Site]],
    criteria: Option[Criteria]
  )(
    implicit
    env: Monad[F],
    querier: Querier,
  ): F[Map[Coding[Site],Either[String,Seq[Query.Match[PatientRecord,Criteria]]]]] = {

    import cats.syntax.apply._

    //TODO: Logging

    val externalResults =
      (sites - Site.local) match {
        case peers if peers.nonEmpty =>
          connector ! (
            PeerToPeerQuery[Criteria,PatientRecord](
              Site.local,
              querier,
              criteria
            ),
            peers
          )

        case _ =>
          Map.empty[Coding[Site],Either[String,Seq[Query.Match[PatientRecord,Criteria]]]]
            .pure[F]
      }


    // Expand the query criteria only here,
    // to save bandwidth transmitting them to peers and
    // to avoid "log pollution" with potentially very long expanded criteria 
    val localResults =
      sites.contains(Site.local) match {
        case true =>
          (db ? criteria.map(CriteriaExpander))
            .map(results => Some(Site.local -> results))

        case _ =>
          None.pure[F]
      }

    (externalResults,localResults)
      .mapN(_ ++ _)

  }
*/

  override def queries(
    implicit
    env: Monad[F],
    querier: Querier
  ): F[Seq[Query[Criteria]]] = {

    log.info(s"Getting current Queries for $querier")

    cache.queries.pure
  }



  override def get(
    id: Query.Id
  )(
    implicit
    env: Monad[F],
    querier: Querier
  ): F[Option[Query[Criteria]]] = {

    log.info(s"Getting Query $id for $querier")

    cache.getQuery(id).pure

  }

  override def resultSet(
    id: Query.Id
  )(
    implicit
    env: Monad[F],
    querier: Querier
  ): F[Option[Results]] = {

    log.info(s"Getting ResultSet of Query $id for $querier")

    cache.getResults(id).pure

  }


  override def patientRecord(
    id: Query.Id,
    patId: Id[Patient]
  )(
    implicit
    env: Monad[F],
    querier: Querier
  ): F[Option[PatientRecord]] = {

    log.info(s"Getting Patient Record $patId of Query $id for $querier")

    cache.getResults(id)
      .flatMap(_.patientRecord(patId))
      .pure
  }


  override def retrievePatientRecord(
    site: Coding[Site],
    patient: Id[Patient],
    snapshot: Option[Long]
  )(
    implicit
    env: Monad[F],
    querier: Querier
  ): F[Either[String,Snapshot[PatientRecord]]] = {

    log.info(
      s"Retrieving Patient Record $patient ${snapshot.map(snp => s"(Snapshot $snp)").getOrElse("")} from Site ${site.code.value} for $querier"
    )

    if (site == Site.local){
      (db ? (patient,snapshot))
        .map(
          _.toRight(s"Invalid Patient ID ${patient.value}${snapshot.map(snp => s" and/or Snapshot ID $snp").getOrElse("")}")
        )

    } else {
      connector ! (
        PatientRecordRequest[PatientRecord](
          Site.local,
          querier,
          patient,
          snapshot
        ),
        site
      )
    }
  }


  override def !(
    req: PeerToPeerQuery[Criteria,PatientRecord]
  )(
    implicit
    env: Monad[F]
  ): F[Either[String,Seq[Query.Match[PatientRecord,Criteria]]]] = {

    log.info(
      s"""Processing peer-to-peer query from site ${req.origin.code.value}
          Querier: ${req.querier.value}
          Criteria:\n${Json.prettyPrint(Json.toJson(req.criteria))}"""
    )

    // Expand the query criteria
    db ? req.criteria.map(CriteriaExpander)

  }


  override def !(
    req: PatientRecordRequest[PatientRecord]
  )(
    implicit
    env: Monad[F]
  ): F[Option[req.ResultType]] = {

    log.info(
      s"""Processing PatientRecord from site ${req.origin.code.value}
          Querier: ${req.querier.value}
          Patient-ID ${req.patient.value}
          Snapshot-ID ${req.snapshot.getOrElse("-")}"""
    )

    db ? (req.patient,req.snapshot)

  }


}
