package de.dnpm.dip.service.query


import java.time.{
  Instant,
  LocalDateTime
}
import cats.Monad
import cats.data.{
  EitherNel,
  Ior,
  IorNel,
  IorT
}
import de.dnpm.dip.util.{
  Logging,
  Completer
}
import de.dnpm.dip.coding.Code
import de.dnpm.dip.coding.Coding
import de.dnpm.dip.model.{
  Id,
  Patient,
  Snapshot,
  Site
}
import play.api.libs.json.{
  Format,
  Reads,
  Writes
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
  F,Monad[F],UseCase,String
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
  protected val cache: QueryCache[Criteria,Filter,Results,PatientRecord] 


  protected implicit val criteriaCompleter: Completer[Criteria]

  // Completer[Criteria] to allow expanding the criteria,
  // e.g. including sub-classes of concepts, etc
  // separately from the completed criteria returned to the client
  protected val CriteriaExpander: Completer[Criteria]


  protected def DefaultFilter(
    rs: Seq[Snapshot[PatientRecord]]
  ): Filter

  protected val ResultSetFrom: (Query.Id,Seq[(Snapshot[PatientRecord],Criteria)]) => Results
 
  protected val preprocess: PatientRecord => PatientRecord  // Complete, etc...


  protected implicit val siteCompleter: Completer[Coding[Site]] =
    Completer.of(
      site =>
       (connector.localSite :: connector.otherSites.toList)
         .collectFirst {
           case coding if coding.code == site.code =>
             site.copy(display = coding.display)
         }
         .getOrElse(site)
    )


  override def sites(
    implicit
    env: Monad[F]
  ): F[Sites] =
    Sites(
      connector.localSite,
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
                s"Invalid PreparedQuery ID ${id.value}"
                  .asLeft[PreparedQuery[Criteria]]
                  .pure
            }

        } yield result.toEitherNel 


      case Delete(id) =>
        preparedQueryDB
          .delete(id)
          .map(
            _.toRight(s"Invalid PreparedQuery ID ${id.value}")
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

    // TODO: Logging

    preparedQueryDB.get(id)

  }


  override def ?(
    q: PreparedQuery.Query
  )(
    implicit
    env: Monad[F],
    querier: Querier
  ): F[Seq[PreparedQuery[Criteria]]] = {

    // TODO: Logging
    
    preparedQueryDB.query(q)
  }


  override def process(
    cmd: Data.Command[PatientRecord]
  )(
    implicit 
    env: Monad[F]
  ): F[Either[String,Data.Outcome[PatientRecord]]] = {

    //TODO: Logging
    
    cmd match {

      case Data.Save(dataSet) =>
        (preprocess andThen db.save)(dataSet)

      case Data.Delete(patient) =>
        db.delete(patient)

    }

  }

/*
  override def !(
    cmd: Query.Command[Criteria,Filter]
  )(
    implicit
    env: Monad[F],
    querier: Querier
  ): F[String IorNel Query[Criteria,Filter]] = {

    cmd match {

      case Query.Submit(md,crit) => {


        log.info(s"Processing new query by $querier") 

        val id = cache.newQueryId

        val mode = md.complete

        val criteria = crit.complete

        (
          for {
            //TODO: parameter validation
            
            results <-
              IorT { executeQuery(id,mode,criteria) }

            query =
              Query[Criteria,Filter](
                id,
                LocalDateTime.now,
                querier,
                mode,
                criteria,
                DefaultFilter(results.map(_._1)),
                cache.timeoutSeconds,
                Instant.now
              )

            _ = cache += (query -> ResultSetFrom(id,results))
 
          } yield query
        )
        .value

      }

      case Query.Update(id,optMode,optCriteria) => {

        log.info(s"Updating Query $id") 
        
        cache.getQuery(id) match {

          case None =>
            s"Invalid Query ID ${id.value}"
              .leftIor[Query[Criteria,Filter]]
              .toIorNel
              .pure[F]

          case Some(query) => {

            //TODO: criteria validation
            //
            val (mode,modeChanged) =
              optMode
                .map(_.complete)
                .pipe(
                  mode => mode.getOrElse(query.mode) -> mode.filter(_ != query.mode).isDefined
                ) 

            val (criteria,criteriaChanged) =
              optCriteria
                .map(_.complete)
                .pipe(
                  crit => crit.getOrElse(query.criteria) -> crit.filter(_ != query.criteria).isDefined
                ) 

            if (modeChanged || criteriaChanged){
              log.debug(s"Query mode or criteria changed, re-submitting...") 
              (
                for {
                  results <-
                    IorT { executeQuery(id,mode,criteria) }
                
                  updatedQuery =
                    query.copy(
                      mode = mode,
                      criteria = criteria,
                      filters = DefaultFilter(results.map(_._1)),
                      lastUpdate = Instant.now
                    )
                
                  _ = cache += (updatedQuery -> ResultSetFrom(updatedQuery.id,results))
                
                } yield updatedQuery
              )
              .value
            } else {
              log.debug(s"Query mode or criteria unchanged, nothing to do") 
              query.rightIor[String]
                .toIorNel
                .pure[F]
            }
          }

        }
      }

      case Query.Delete(id) => {

        // Logging

        cache.getQuery(id) match {

          case None =>
            s"Invalid Query ID ${id.value}"
              .leftIor[Query[Criteria,Filter]]
              .toIorNel
              .pure[F]

          case Some(query) => {
            cache -= id
            query.rightIor[String]
              .toIorNel
              .pure
          }

        }

      }

    }

  }

  private def executeQuery(
    id: Query.Id,
    mode: Coding[Query.Mode.Value],
    criteria: Criteria
  )(
    implicit
    env: Monad[F],
    querier: Querier,
  ): F[String IorNel Seq[(Snapshot[PatientRecord],Criteria)]] = {

    import cats.syntax.apply._
    import cats.instances.list._
    import Query.Mode.{Local,Federated}

    //TODO: Logging


    val externalResults =
      mode match {
        case Query.Mode(Federated) =>
          for {
            results <-
              connector ! PeerToPeerQuery[Criteria,PatientRecord](
                connector.localSite,
                querier,
                criteria
              )
          } yield
            results.foldLeft(
               Seq.empty[(Snapshot[PatientRecord],Criteria)].rightIor[String].toIorNel
            ){
              case (acc,(_,result)) =>
                acc combine result.toIor.toIorNel      
            }        

        case Query.Mode(Local) | _ =>
          Seq.empty[(Snapshot[PatientRecord],Criteria)]
            .rightIor[String]
            .toIorNel
            .pure[F]
      }


    // Expand the query criteria only here,
    // to save bandwidth transmitting them to peers and
    // to avoid "log pollution" with potentially very long expanded criteria 
    val localResults =
      (db ? CriteriaExpander(criteria)).map(_.toIor.toIorNel)

    (localResults,externalResults).mapN(_ combine _)

  }
*/


  import Query.Mode.{Local,Federated}

  override def !(
    cmd: Query.Command[Criteria,Filter]
  )(
    implicit
    env: Monad[F],
    querier: Querier
  ): F[String IorNel Query[Criteria,Filter]] = {

    import cats.syntax.apply._
    import cats.instances.list._

    def modeAndSites(
      optMode: Option[Coding[Query.Mode.Value]],
      optSites: Option[Set[Coding[Site]]]
    ): (Coding[Query.Mode.Value],Set[Coding[Site]]) =
      (optMode,optSites) match {
        case (_,Some(sites)) =>
          (
            (sites - connector.localSite) match { 
              case s if s.nonEmpty => Coding(Federated)
              case _               => Coding(Local)
            },
            sites
          )

        case (Some(mode),None) => 
          (
            mode.complete,
            mode match { 
              case Query.Mode(Federated) => connector.otherSites + connector.localSite
              case _                     => Set(connector.localSite)
            }
          )

        case _ =>
          (
            Coding(Federated),
            connector.otherSites + connector.localSite
          )

      }


    cmd match {

      case Query.Submit(optMode,optSites,crit) => {

        log.info(s"Processing new query by $querier") 

        val id =
          cache.newQueryId

        val (mode,sites) =
          modeAndSites(optMode,optSites.map(_.complete))

        //TODO: criteria validation
        val criteria =
          crit.complete

        for {
          resultsBySite <-
            executeQuery(id,sites,criteria) 

          siteStatus =
            resultsBySite.map {
              case (site,result) => Entry(site,result.isRight)
            }
            .toSeq

          errsOrResults =
            resultsBySite
              .values
              .map(_.toIor.toIorNel)
              .reduce(_ combine _)

          errsOrQuery =
            errsOrResults.map {
              results =>
                Query[Criteria,Filter](
                  id,
                  LocalDateTime.now,
                  querier,
                  mode,
                  siteStatus,
                  criteria,
                  DefaultFilter(results.map(_._1)),
                  cache.timeoutSeconds,
                  Instant.now
                )
                .tap(            
                  q => cache += (q -> ResultSetFrom(id,results))
                )
            }
 
        } yield errsOrQuery

      }

      case Query.Update(id,optMode,optSites,optCriteria) => {

        log.info(s"Updating Query $id") 
        
        cache.getQuery(id) match {

          case None =>
            s"Invalid Query ID ${id.value}"
              .leftIor[Query[Criteria,Filter]]
              .toIorNel
              .pure[F]

          case Some(query) => {

            val (mode,sites) =
              modeAndSites(optMode,optSites.map(_.complete))

            val sitesChanged =
              sites != query.siteStatus.map(_.key).toSet

            //TODO: criteria validation
              
            val (criteria,criteriaChanged) =
              optCriteria
                .map(_.complete)
                .pipe(
                  crit => crit.getOrElse(query.criteria) -> crit.filter(_ != query.criteria).isDefined
                ) 

            if (sitesChanged || criteriaChanged){

              log.debug(s"Query target sites or criteria changed, re-submitting...") 

              for {
                resultsBySite <-
                  executeQuery(id,sites,criteria) 
              
                siteStatus =
                  resultsBySite.map {
                    case (site,result) => Entry(site,result.isRight)
                  }
                  .toSeq
              
                errsOrResults =
                  resultsBySite
                    .values
                    .map(_.toIor.toIorNel)
                    .reduce(_ combine _)
              
                errsOrQuery =
                  errsOrResults.map {
                    results =>
                      query.copy(
                        mode = mode,
                        siteStatus = siteStatus,
                        criteria = criteria,
                        filters = DefaultFilter(results.map(_._1)),
                        lastUpdate = Instant.now
                      )
                      .tap(            
                        q => cache += (q -> ResultSetFrom(id,results))
                      )
                  }
              
              } yield errsOrQuery

            } else {
              log.debug(s"Query target sites or criteria unchanged, nothing to do") 
              query.rightIor[String]
                .toIorNel
                .pure[F]
            }

          }
        }
      }

      case Query.Delete(id) => {

        // Logging

        cache.getQuery(id) match {

          case None =>
            s"Invalid Query ID ${id.value}"
              .leftIor[Query[Criteria,Filter]]
              .toIorNel
              .pure[F]

          case Some(query) => {
            cache -= id
            query.rightIor[String]
              .toIorNel
              .pure[F]
          }

        }

      }

    }

  }


  private def executeQuery(
    id: Query.Id,
    sites: Set[Coding[Site]],
    criteria: Criteria
  )(
    implicit
    env: Monad[F],
    querier: Querier,
  ): F[Map[Coding[Site],Either[String,Seq[(Snapshot[PatientRecord],Criteria)]]]] = {

    import cats.syntax.apply._
    import cats.instances.list._

    //TODO: Logging

    val externalResults =
      (sites - connector.localSite) match {
        case peers if peers.nonEmpty =>
          connector ! (
            PeerToPeerQuery[Criteria,PatientRecord](
              connector.localSite,
              querier,
              criteria
            ),
            peers
          )

        case _ =>
          Map.empty[Coding[Site],Either[String,Seq[(Snapshot[PatientRecord],Criteria)]]]
            .pure[F]
      }


    // Expand the query criteria only here,
    // to save bandwidth transmitting them to peers and
    // to avoid "log pollution" with potentially very long expanded criteria 
    val localResults =
      sites.contains(connector.localSite) match {
        case true =>
          (db ? CriteriaExpander(criteria))
            .map(results => Some(connector.localSite -> results))

        case _ =>
          None.pure[F]
      }

    (externalResults,localResults)
      .mapN(_ ++ _)

  }

/*
  private def executeQuery(
    id: Query.Id,
    sites: Set[Coding[Site]],
    criteria: Criteria
  )(
    implicit
    env: Monad[F],
    querier: Querier,
  ): F[Map[Coding[Site],Either[String,Seq[(Snapshot[PatientRecord],Criteria)]]]] = {

    import cats.syntax.apply._
    import cats.instances.list._

    //TODO: Logging

    val externalResults =
      (sites - connector.localSite) match {
        case peers if peers.nonEmpty =>
          connector ! (
            PeerToPeerQuery[Criteria,PatientRecord](
              connector.localSite,
              querier,
              criteria
            ),
            peers
          )

        case _ =>
          Map.empty[Coding[Site],Either[String,Seq[(Snapshot[PatientRecord],Criteria)]]]
            .pure[F]
      }


    // Expand the query criteria only here,
    // to save bandwidth transmitting them to peers and
    // to avoid "log pollution" with potentially very long expanded criteria 
    val localResults =
      sites.contains(connector.localSite) match {
        case true =>
          (db ? CriteriaExpander(criteria))
            .map(results => Some(connector.localSite -> results))

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
  ): F[Seq[Query[Criteria,Filter]]] = {

    //TODO: Logging

    cache.queries.pure
  }



  override def get(
    id: Query.Id
  )(
    implicit
    env: Monad[F],
    querier: Querier
  ): F[Option[Query[Criteria,Filter]]] = {

// TODO: Logging 

    Monad[F].pure {
      cache.getQuery(id)
    }

  }

  override def resultSet(
    id: Query.Id
  )(
    implicit
    env: Monad[F],
    querier: Querier
  ): F[Option[Results]] = {

// TODO: Logging 

    Monad[F].pure {
      cache.getResults(id)
    }

  }


  import scala.language.implicitConversions

  protected implicit def toPredicate(filter: Filter): PatientRecord => Boolean

  override def summary(
    id: Query.Id,
    filter: Filter,
  )(
    implicit
    env: Monad[F],
    querier: Querier,
  ): F[Option[Results#SummaryType]] =
    resultSet(id)
      .map(
        _.map(_.summary(filter))
      )


  override def patientMatches(
    id: Query.Id,
    filter: Filter,
  )(
    implicit
    env: Monad[F],
    querier: Querier,
  ): F[Option[Seq[PatientMatch[Criteria]]]] = 
    resultSet(id)
      .map(
        _.map(_.patientMatches(filter).asInstanceOf[Seq[PatientMatch[Criteria]]])
      )


  override def patientRecord(
    id: Query.Id,
    patId: Id[Patient]
  )(
    implicit
    env: Monad[F],
    querier: Querier
  ): F[Option[PatientRecord]] = {

//TODO: Logging

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

    //TODO: Logging

    if (site == connector.localSite){
      (db ? (patient,snapshot))
        .map(
          _.toRight(s"Invalid Patient ID ${patient.value}${snapshot.map(snp => s" and/or Snapshot ID $snp").getOrElse("")}")
        )

    } else {
      connector ! (
        PatientRecordRequest[PatientRecord](
          connector.localSite,
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
  ): F[Either[String,Seq[(Snapshot[PatientRecord],Criteria)]]] = {

    log.info(
      s"""Processing peer-to-peer query from site ${req.origin.code.value}
          Querier: ${req.querier.value}
          Criteria: ${req.criteria}"""
    )

    // Expand the query criteria
    db ? CriteriaExpander(req.criteria)

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
