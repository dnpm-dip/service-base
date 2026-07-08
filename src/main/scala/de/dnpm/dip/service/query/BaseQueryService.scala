package de.dnpm.dip.service.query


import java.time.{
  Instant,
  LocalDateTime
}
import java.util.UUID.randomUUID
import scala.concurrent.duration._
import cats.Monad
import cats.data.EitherNel
import cats.syntax.traverse._
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
  Cache,
  Connector,
  ConnectionStatus,
}
import de.dnpm.dip.service.controlling.{
  Controlling,
  PatientDataCounts
}
import play.api.libs.json.{
  Json,
  Format,
  Reads
}


abstract class BaseQueryService[F[+_],UseCase <: UseCaseConfig](
  implicit 
  fpr: Format[UseCase#PatientRecord],
  fcrit: Format[UseCase#Criteria]
)
extends QueryService[F,Monad[F],UseCase]
with Logging
{

  import scala.util.chaining._
  import cats.syntax.either._
  import cats.syntax.ior._
  import cats.syntax.applicative._
  import cats.syntax.functor._
  import cats.syntax.flatMap._
  import de.dnpm.dip.util.Completer.syntax._
  import BaseQueryService.FEDERATED_QUERIES_INACTIVE
  import Query.Mode.{Local,Federated,Custom}
  import QueryService._


  
  protected val preparedQueryDB: PreparedQueryDB[F,Monad[F],Criteria,String]
  protected val db: LocalDB[F,Monad[F],Criteria,PatientRecord]
  protected val connector: Connector[F,Monad[F]]
  protected val querySessions: Cache[Query.Id,(Query[Criteria],Results)] 

  protected implicit val criteriaCompleter: Completer[Criteria]

  protected def validate(criteria: Criteria): EitherNel[String,Criteria]


  // Completer[Criteria] to allow expanding the criteria,
  // e.g. including sub-classes of concepts, etc
  // separately from the completed criteria returned to the client
  protected val CriteriaExpander: Completer[Criteria]


  protected def ResultSetFrom(
    query: Query[Criteria],
    results: Seq[Query.Match[PatientRecord,Criteria]]
  ): Results
 

  protected implicit val siteCompleter: Completer[Coding[Site]] = {

    val sites = Site.local :: connector.otherSites.toList

    site => sites.find(_.code == site.code).getOrElse(site)
  }


  override def patientDataCounts(
    criteria: Option[Controlling.Criteria]
  )(
    implicit env: Monad[F]
  ): F[PatientDataCounts] = {

    log.info(s"Query: compiling PatientDataCounts, criteria: ${criteria.map(_.toString).getOrElse("-")}")

    db.patientDataCounts(criteria)
  }

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
  ): F[Either[Query.Error,PreparedQuery[Criteria]]] = {

    import de.dnpm.dip.util.Operations.syntax._
    import PreparedQuery.{Create,Update,Delete}

    cmd match {

      case Create(name,rawCriteria) =>
        log.info(s"Processing new PreparedQuery by $querier")

        validate(rawCriteria) match {
          case Right(criteria) =>
            for { 
              id <- preparedQueryDB.newId
            
              pq = PreparedQuery(
                id,
                querier,
                name,
                criteria.complete,
                LocalDateTime.now,
                Instant.now
              )
            
              result <- preparedQueryDB.save(pq).map(
                _.bimap(
                  Query.GenericError(_),
                  _ => pq
                )
              )
            
            } yield result

          case Left(errors) =>
            Query.InvalidCriteria(errors).asLeft.pure[F]
        }


      case Update(id,optName,optRawCriteria) =>

        log.info(s"Updating PreparedQuery $id by $querier")

        optRawCriteria.traverse(validate) match {

          case Right(optCriteria) =>
            for {
              optPq <- preparedQueryDB.get(id)
            
              optUpdated = optPq.map(
                _.patch(
                  optName.map(name => _.copy(name = name)),
                  optCriteria.map(crit => _.copy(criteria = crit.complete)),
                )
                .update(
                  _.copy(lastUpdate = Instant.now)
                )
              )
            
              result <- optUpdated match {
                case Some(updated) =>
                  preparedQueryDB.save(updated).map(
                    _.bimap(Query.GenericError(_),_ => updated)
                  )
            
                case None => 
                  Query.InvalidId.asLeft
                    .pure
              }
            
            } yield result

          case Left(errors) =>
            Query.InvalidCriteria(errors).asLeft.pure[F]

        }


      case Delete(id) =>

        log.info(s"Deleting PreparedQuery $id by $querier")

        preparedQueryDB
          .delete(id)
          .map(_.toRight(Query.InvalidId))

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




  protected val sessionTimeout = 10 minutes

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

      case submit @ Query.Submit(optMode,optSites,optCriteria) => {

        log.info(s"Processing new query by $querier: \n${Json.prettyPrint(Json.toJson(submit))}") 

        // Criteria validation
        optCriteria.traverse(validate).map(_.complete) match {

          case Right(criteria) =>
            val id = Query.Id(randomUUID.toString)

            val (mode,sites) = modeAndSites(optMode,optSites.complete)

            for {
              resultsBySite <- executeQuery(id,sites,criteria) 
            
              errsOrResults =
                resultsBySite
                  .values
                  .map(_.toIor.toIorNel)
                  .reduceOption(_ combine _)
                  .getOrElse(Seq.empty.rightIor)
                  .toEither
            
              errsOrQuery = errsOrResults match {
                case Right(results) if (results.nonEmpty) =>
                  Query[Criteria](
                    id,
                    LocalDateTime.now,
                    querier,
                    mode,
                    ConnectionStatus.from(resultsBySite),
                    criteria,
                    sessionTimeout.toSeconds.toInt,
                    Instant.now
                  )
                  .tap(query => querySessions.put(id,query -> ResultSetFrom(query,results), sessionTimeout))
                  .asRight
            
                case Right(_) => Query.NoResults.asLeft
                    
                case Left(errs) => Query.ConnectionErrors(errs).asLeft
              }
            
            } yield errsOrQuery

          case Left(errors) =>
            Query.InvalidCriteria(errors).asLeft.pure[F]
        }
      }


      case update @ Query.Update(id,optMode,optSites,optRawCriteria) => {

        log.info(s"Updating Query $id by $querier: \n${Json.prettyPrint(Json.toJson(update))}") 
        
        querySessions.get(id).map(_._1) match {

          case Some(query) =>

            val (mode,sites) = modeAndSites(optMode.getOrElse(query.mode),optSites.complete)

            optRawCriteria.traverse(validate).map(_.complete) match {
              
              case Right(optCriteria) =>

                val sitesChanged = sites != query.peers.map(_.site).toSet

                if (sitesChanged || optCriteria.exists(c => query.criteria.contains(c))){
                
                  log.debug(s"Query target sites or criteria changed, re-submitting...") 
                
                  for {
                    resultsBySite <- executeQuery(id,sites,optCriteria) 
                  
                    errsOrResults =
                      resultsBySite
                        .values
                        .map(_.toIor.toIorNel)
                        .reduceOption(_ combine _)
                        .getOrElse(Seq.empty.rightIor)
                        .toEither
                
                    errsOrQuery = errsOrResults match {
                      case Right(results) if (results.nonEmpty) =>

                        val updatedQuery = query.copy(
                          mode = mode,
                          criteria = optCriteria.orElse(query.criteria),
                          peers = ConnectionStatus.from(resultsBySite),
                          lastUpdate = Instant.now
                        )

                        querySessions.put(id,updatedQuery -> ResultSetFrom(updatedQuery,results),sessionTimeout)
                        updatedQuery.asRight
                
                      case Right(_) => Query.NoResults.asLeft
                
                      case Left(errs) => Query.ConnectionErrors(errs).asLeft
                    }
                  
                  } yield errsOrQuery
                  
                } else {
                  log.debug(s"Query target sites or criteria unchanged, nothing to do") 
                  query.asRight.pure[F]
                }

              case Left(errors) =>
               Query.InvalidCriteria(errors).asLeft.pure[F]

            }

          case None => Query.InvalidId.asLeft.pure[F]

        }

      }

      case Query.Delete(id) => 
        log.info(s"Deleting Query $id by $querier") 
        querySessions.remove(id) match { 
          case None => Query.InvalidId.asLeft.pure[F]
          case Some((query,_)) => query.asRight.pure[F]
        }

    }

  }


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

    val externalResults =
      (sites - Site.local) match {
        case peers if federatedQueriesActive && peers.nonEmpty =>
          connector ! (
            FederatedQuery[Criteria,PatientRecord](
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


  override def queries(
    implicit
    env: Monad[F],
    querier: Querier
  ): F[Seq[Query[Criteria]]] = {

    log.info(s"Getting current Queries for $querier")

    querySessions.filter((_,_) => true).values.map(_._1).toSeq.pure
  }



  override def get(
    id: Query.Id
  )(
    implicit
    env: Monad[F],
    querier: Querier
  ): F[Option[Query[Criteria]]] = {

    log.info(s"Getting Query $id for $querier")

    querySessions.get(id).map(_._1).pure
  }

  override def resultSet(
    id: Query.Id
  )(
    implicit
    env: Monad[F],
    querier: Querier
  ): F[Option[Results]] = {

    log.info(s"Getting ResultSet of Query $id for $querier")

    querySessions.get(id).map(_._2).pure

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

    querySessions.get(id)
      .map(_._2)
      .flatMap(_.patientRecord(patId))
      .pure
  }


  override def retrievePatientRecord(
    targetSite: Coding[Site],
    patient: Id[Patient],
    snapshot: Option[Long]
  )(
    implicit
    env: Monad[F],
    querier: Querier
  ): F[Either[String,Option[Snapshot[PatientRecord]]]] = {

    // Explicit Reads[Option[_]] required here because apparently, implicit resolution of Reads[Option[_]]
    // doesn't work at the top-level, which sort of makes sense:
    // Reading an optional field on a top-level DTO-type T has a clear rationale.
    // However, expecting/reading a Option[T] as top-level JSON structure is a bit unusual,
    // but required here, in order to use PeerToPeerRequest.ResultType consistently
    implicit lazy val readsOptPatRec = Reads.optionWithNull[Snapshot[PatientRecord]]

    log.info(
      s"Retrieving Patient Record $patient ${snapshot.map(snp => s"(Snapshot $snp)").getOrElse("")} from Site ${targetSite.code} for $querier"
    )

    if (targetSite == Site.local)
      (db ? (patient,snapshot)).map(_.asRight)

    else
      connector ! (
        PatientRecordRequest[PatientRecord](
          Site.local,
          querier,
          patient,
          snapshot
        ),
        targetSite
      )
    
  }


  override def !(
    query: FederatedQuery[Criteria,PatientRecord]
  )(
    implicit env: Monad[F]
  ): F[Either[String,query.ResultType]] = 
    if (federatedQueriesActive){

      log.info(
        s"""Processing peer-to-peer query from site ${query.origin.code.value}, Querier: ${query.querier.value}, Criteria:\n${Json.prettyPrint(Json.toJson(query.criteria))}"""
      )
      
      // Expand the query criteria
      db ? query.criteria.map(CriteriaExpander)

    } else {
      FEDERATED_QUERIES_INACTIVE.asLeft.pure
    }


  override def !(
    req: PatientRecordRequest[PatientRecord]
  )(
    implicit env: Monad[F]
  ): F[Either[String,req.ResultType]] = 
    if (federatedQueriesActive){

      log.info(
        s"""Processing PatientRecord from site ${req.origin.code.value}
            Querier: ${req.querier.value}
            Patient-ID ${req.patient.value}
            Snapshot-ID ${req.snapshot.getOrElse("-")}"""
      )
    
      (db ? (req.patient,req.snapshot)).map(_.asRight)

    } else {
      FEDERATED_QUERIES_INACTIVE.asLeft.pure
    }

}


object BaseQueryService
{
  val FEDERATED_QUERIES_INACTIVE = "Federated Queries not activated"
}
