package de.dnpm.dip.service.query


import java.time.{
  Instant,
  LocalDateTime
}
import cats.Monad
import cats.data.{
  EitherNel,
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


  override def sites: List[Coding[Site]] =
    connector.localSite :: connector.otherSites


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
                optCriteria.map(crit => _.copy(criteria = crit)),
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


  override def process(
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

            val mode =
              optMode.map(_.complete).getOrElse(query.mode)

            val criteria =
              optCriteria.map(_.complete).getOrElse(query.criteria)

            //TODO: parameter validation

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

          }
        }
      }

/*      
      case Query.ApplyFilter(id,filters) => {

        cache.get(id) match {

          case None =>
            s"Invalid Query ID ${id.value}"
              .leftIor[Query[Criteria,Filter]]
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
*/

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

    val expandedCriteria =
      CriteriaExpander(criteria)

    val externalResults =
      mode match {
        case Query.Mode(Federated) =>
          for {
            results <-
              connector ! PeerToPeerQuery[Criteria,PatientRecord](
                connector.localSite,
                querier,
                expandedCriteria
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


    val localResults =
      (db ? expandedCriteria).map(_.toIor.toIorNel)

    (localResults,externalResults).mapN(_ combine _)

  }


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
