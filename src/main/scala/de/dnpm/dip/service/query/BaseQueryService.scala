package de.dnpm.dip.service.query


import java.time.{
  Instant,
  LocalDateTime
}
import cats.Monad
import cats.data.{IorNel,IorT}
import de.dnpm.dip.util.Logging
import de.dnpm.dip.coding.Coding
import de.dnpm.dip.model.{
  Id,
  Patient,
  Snapshot,
  Site
}



trait BaseQueryService[
  F[+_],
  UseCase <: UseCaseConfig,
]
extends Data.Ops[
  F,Monad[F],UseCase,String
]
with QueryOps[
  F,Monad[F],UseCase,String
]
with Logging
{

  import cats.syntax.either._
  import cats.syntax.ior._
  import cats.syntax.applicative._
  import cats.syntax.functor._
  import cats.syntax.flatMap._
  
  protected val localDB: LocalDB[F,Monad[F],Parameters,PatientRecord]
  protected val connector: Connector[F,Monad[F]]
//  protected val connector: Connector[F,Monad[F],Parameters,PatientRecord]
  protected val cache: QueryCache[Parameters,Filters,Results,PatientRecord] 


  protected def DefaultFilters(
    rs: Seq[Snapshot[PatientRecord]]
  ): Filters


  protected val ResultSetFrom: Results#BuilderFrom[PatientRecord]


  protected def toPredicate(
    flts: Filters
  ): PatientRecord => Boolean


  protected val preprocess: PatientRecord => PatientRecord 


  override def process(
    cmd: Data.Command[PatientRecord]
  )(
    implicit 
    env: Monad[F]
  ): F[Either[String,Data.Outcome[PatientRecord]]] = {

    //TODO: Logging
    
    cmd match {

      case Data.Save(dataSet) =>
        (preprocess andThen localDB.save)(dataSet)

      case Data.Delete(patient) =>
        localDB.delete(patient)

    }

  }


  override def process(
    cmd: Query.Command[Parameters,Filters]
  )(
    implicit
    env: Monad[F],
    querier: Querier
  ): F[String IorNel Query[Parameters,Filters]] = {

    cmd match {

      case Query.Submit(mode,parameters) => {

        log.info(s"Processing new query by $querier") 

        val id = cache.newQueryId

        (
          for {
            //TODO: parameter validation

            results <-
              IorT { executeQuery(id,mode,parameters) }

            query =
              Query[Parameters,Filters](
                id,
                LocalDateTime.now,
                querier,
                mode,
                parameters,
                DefaultFilters(results),
                Instant.now
              )

            _ = cache += (query,ResultSetFrom(id,results.map(_.data)),results)
 
          } yield query
        )
        .value

      }

      case Query.Update(id,optMode,optParameters) => {

        log.info(s"Updating Query $id") 
        
        cache.getQuery(id) match {

          case None =>
            s"Invalid Query ID ${id.value}"
              .leftIor[Query[Parameters,Filters]]
              .toIorNel
              .pure[F]

          case Some(query) => {

            val mode       = optMode.getOrElse(query.mode)
            val parameters = optParameters.getOrElse(query.parameters)

            //TODO: parameter validation

            (
              for {
                results <-
                  IorT { executeQuery(id,mode,parameters) }
              
                updatedQuery =
                  query.copy(
                    mode = mode,
                    parameters = parameters,
                    filters = DefaultFilters(results),
                    lastUpdate = Instant.now
                  )
              
                _ = cache += (updatedQuery,ResultSetFrom(updatedQuery.id,results.map(_.data)),results)
              
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
              .leftIor[Query[Parameters,Filters]]
              .toIorNel
              .pure[F]

          case Some((query,_,results)) => {
              
            for {
              updatedQuery <-
                Monad[F].pure {
                  query.copy(
                   filters = filters,
                   lastUpdate = Instant.now
                  )
                }

              filteredResults =
                results.filter(
                  toPredicate(filters) compose (_.data)
                )

              _ =
                cache += (updatedQuery,ResultSetFrom(updatedQuery.id,filteredResults.map(_.data)),filteredResults)
              
            } yield updatedQuery.rightIor[String].toIorNel

          }

        }

      }

    }

  }


  private def executeQuery(
    id: Query.Id,
    mode: Query.Mode.Value,
    parameters: Parameters
  )(
    implicit
    env: Monad[F],
    querier: Querier
  ): F[IorNel[String,Seq[Snapshot[PatientRecord]]]] = {

    import cats.syntax.apply._
    import cats.instances.list._
    import Query.Mode._

    //TODO: Logging

    val externalResults =
      mode match {

        case Federated =>
          for {
            results <- connector ! PeerToPeerQuery[Parameters,PatientRecord](connector.localSite,querier,parameters)
          } yield
            results.foldLeft(
               Seq.empty[Snapshot[PatientRecord]].rightIor[String].toIorNel
            ){
              case (acc,(site,result)) =>
                acc combine result.leftMap(err => s"Error from site ${site.display}: $err").toIor.toIorNel      
            }
         
        case Local =>
          Seq.empty[Snapshot[PatientRecord]]
            .rightIor[String]
            .toIorNel
            .pure[F]
     }

     val localResults =
       (localDB ? parameters).map(_.toIor.toIorNel)

     (localResults,externalResults).mapN(_ combine _)

  }


  override def get(
    id: Query.Id
  )(
    implicit
    env: Monad[F],
    querier: Querier
  ): F[Option[Query[Parameters,Filters]]] = {

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



  override def getPatientRecord(
    site: Coding[Site],
    patient: Id[Patient],
    snapshot: Option[Id[Snapshot[PatientRecord]]]
  )(
    implicit
    env: Monad[F],
    querier: Querier
  ): F[Either[String,Snapshot[PatientRecord]]] = {

    //TODO: Logging

    if (site == connector.localSite){
      (localDB ? (patient,snapshot))
        .map(
          _.toRight(s"Invalid Patient Id ${patient.value}${snapshot.map(snp => s" and/or Snapshot ID ${snp.value}").getOrElse("")}")
        )

    } else {
      connector ! (
        PatientRecordRequest(
          connector.localSite,
          querier,
          patient,
          snapshot
        ),
        site
      )
    }
  }



/*
  override def !(
    req: PeerToPeerRequest
  )(
    implicit
    env: Monad[F]
  ): F[Either[String,PeerToPeerResponse[req.ResultType]]] = {

    import scala.util.chaining._

    log.info(s"Processing peer-to-peer query from site ${req.origin.code.value}")

    req match {

      case PeerToPeerQuery[Parameters,PatientRecord](_,querier,parameters) =>

        querier match {
        
          case Some(Querier(q)) =>
        
            log.info(s"Querier: $q\n Parameters: ${parameters}")
        
            (localDB ? parameters).map(PeerToPeerResponse(_))
        
          case None =>
            "Missing Querier ID!"
              .tap(log.error)
              .asLeft[Seq[Snapshot[PatientRecord]]]
              .pure[F]
        }
    }

  }
*/


  override def !(
    req: PeerToPeerQuery[Parameters,PatientRecord]
  )(
    implicit
    env: Monad[F]
  ): F[Either[String,Seq[Snapshot[PatientRecord]]]] = {

    import scala.util.chaining._

    log.info(
      s"""Processing peer-to-peer query from site ${req.origin.code.value}
          Querier: ${req.querier.value}
          Parameters: ${req.parameters}"""
    )

    localDB ? req.parameters

/*
    log.info(s"Processing peer-to-peer query from site ${req.origin.code.value}")

    req.querier match {

      case Some(Querier(q)) =>

        log.info(s"Querier: $q  Parameters: ${req.parameters}")

        localDB ? req.parameters

      case None =>
        "Missing Querier ID!"
          .tap(log.error)
          .asLeft[Seq[Snapshot[PatientRecord]]]
          .pure[F]
    }
 */

  }


}
