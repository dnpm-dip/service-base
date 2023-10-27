package de.dnpm.dip.service.query


import java.time.{
  Instant,
  LocalDateTime
}
import cats.Monad
import cats.data.{IorNel,IorT}
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

  import cats.syntax.either._
  import cats.syntax.ior._
  import cats.syntax.applicative._
  import cats.syntax.functor._
  import cats.syntax.flatMap._
  
  protected val db: LocalDB[F,Monad[F],Criteria,PatientRecord]
  protected val connector: Connector[F,Monad[F]]
  protected val cache: QueryCache[Criteria,Filters,Results,PatientRecord] 


  protected implicit val criteriaCompleter: Completer[Criteria]

//  protected implicit val patientRecordCompleter: Completer[PatientRecord]


  protected def DefaultFilters(
    rs: Seq[Snapshot[PatientRecord]]
  ): Filters


  protected val ResultSetFrom: (Query.Id,Seq[(Snapshot[PatientRecord],Criteria)]) => Results


  protected def toPredicate(
    flts: Filters
  ): PatientRecord => Boolean

 
  protected val preprocess: PatientRecord => PatientRecord  // Complete, etc...


  override def sites: List[Coding[Site]] =
    connector.localSite :: connector.otherSites


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


  override def process(
    cmd: Query.Command[Criteria,Filters]
  )(
    implicit
    env: Monad[F],
    querier: Querier
  ): F[String IorNel Query[Criteria,Filters]] = {

    import de.dnpm.dip.util.Completer.syntax._

    cmd match {

      case Query.Submit(mode,criteria) => {


        log.info(s"Processing new query by $querier") 

        val id = cache.newQueryId

        (
          for {
            //TODO: parameter validation

            results <-
              IorT { executeQuery(id,mode,criteria) }

            query =
              Query[Criteria,Filters](
                id,
                LocalDateTime.now,
                querier,
                mode.complete,
                criteria.complete,
                DefaultFilters(results.map(_._1)),
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
              .leftIor[Query[Criteria,Filters]]
              .toIorNel
              .pure[F]

          case Some(query) => {

            val mode     = optMode.getOrElse(query.mode)
            val criteria = optCriteria.getOrElse(query.criteria)

            //TODO: parameter validation

            (
              for {
                results <-
                  IorT { executeQuery(id,mode,criteria) }
              
                updatedQuery =
                  query.copy(
                    mode = mode.complete,
                    criteria = criteria.complete,
                    filters = DefaultFilters(results.map(_._1)),
                    lastUpdate = Instant.now
                  )
              
                _ = cache += (updatedQuery -> ResultSetFrom(updatedQuery.id,results))
              
              } yield updatedQuery
            )
            .value

          }
        }
      }

      case Query.ApplyFilters(id,filters) => {

        cache.get(id) match {

          case None =>
            s"Invalid Query ID ${id.value}"
              .leftIor[Query[Criteria,Filters]]
              .toIorNel
              .pure[F]

          case Some(query -> resultSet) => {
              
            for {
              updatedQuery <-
                Monad[F].pure {
                  query.copy(
                   filters = filters,
                   lastUpdate = Instant.now
                  )
                }

              _ = cache += updatedQuery -> resultSet.withFilter(toPredicate(filters))
              
            } yield updatedQuery.rightIor[String].toIorNel

          }

        }

      }

      case Query.Delete(id) => {

        // Logging

        cache.getQuery(id) match {

          case None =>
            s"Invalid Query ID ${id.value}"
              .leftIor[Query[Criteria,Filters]]
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
  ): F[IorNel[String,Seq[(Snapshot[PatientRecord],Criteria)]]] = {

    import cats.syntax.apply._
    import cats.instances.list._

    //TODO: Logging


    val externalResults =
      if (mode.code.value == Query.Mode.Federated)
        for {
          results <-
            connector ! PeerToPeerQuery[Criteria,PatientRecord](connector.localSite,querier,criteria)
        } yield
          results.foldLeft(
             Seq.empty[(Snapshot[PatientRecord],Criteria)].rightIor[String].toIorNel
          ){
            case (acc,(site,result)) =>
              acc combine result.leftMap(err => s"Error from site ${site.display}: $err").toIor.toIorNel      
          }        
      else
        Seq.empty[(Snapshot[PatientRecord],Criteria)]
          .rightIor[String]
          .toIorNel
          .pure[F]


    val localResults =
      (db ? criteria).map(_.toIor.toIorNel)

    (localResults,externalResults).mapN(_ combine _)

  }


  override def queries(
    implicit
    env: Monad[F],
    querier: Querier
  ): F[Seq[Query[Criteria,Filters]]] = {

    //TODO: Logging

    cache.queries.pure
  }



  override def get(
    id: Query.Id
  )(
    implicit
    env: Monad[F],
    querier: Querier
  ): F[Option[Query[Criteria,Filters]]] = {

// TODO: Logging 

    Monad[F].pure {
      cache.getQuery(id)
    }

  }


  override def summary(
    id: Query.Id
  )(
    implicit
    env: Monad[F],
    querier: Querier
  ): F[Option[Results#Summary]] = {

// TODO: Logging 
   
    for {
      rs <- resultSet(id)
    } yield rs.map(_.summary)

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

    db ? req.criteria

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
