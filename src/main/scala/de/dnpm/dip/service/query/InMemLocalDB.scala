package de.dnpm.dip.service.query


import scala.util.Either
import scala.collection.concurrent.{
  Map,
  TrieMap
}
import cats.Applicative
import de.dnpm.dip.util.Logging
import de.dnpm.dip.model.{
  Id,
  Patient,
  PatientRecord,
  Snapshot
}
import QueryService.{
  Saved,
  Deleted
}
import de.dnpm.dip.service.controlling.{
  Controlling,
  PatientDataCounts
}


class InMemLocalDB[
  F[_],
  C[M[_]] <: Applicative[M],
  Criteria,
  T <: PatientRecord
](
  val criteriaMatcher: Criteria => (T => Option[Criteria])
)
extends LocalDB[F,C[F],Criteria,T]
with Logging
{

  import cats.syntax.functor._
  import cats.syntax.applicative._
  import cats.syntax.either._


  private val cache: Map[Id[Patient],List[Snapshot[T]]] =
    TrieMap.empty



  override def save(
    dataSet: T
  )(
    implicit env: C[F]
  ): F[Either[String,Saved[T]]] = {
  
    //TODO: Logging
   
    val snp = Snapshot.of(dataSet)

    cache.updateWith(dataSet.patient.id){
      case Some(snps) => Some(snp :: snps)
      case None       => Some(List(snp))
    }

    Saved(snp)
      .asRight[String]
      .pure

  }


  override def delete(
    patId: Id[Patient],
  )(
    implicit env: C[F]
  ): F[Either[String,Deleted]] = {

    log.info(s"Deleting all patient record snapshots for Patient ${patId.value}")

    cache -= patId

    Deleted(patId)
      .asRight[String]
      .pure[F]

  }


  override def ?(
    criteria: Option[Criteria]
  )(
    implicit env: C[F]
  ): F[Either[String,Seq[Query.Match[T,Criteria]]]] = {

    criteria.fold(
      cache.values
        .collect {
          case snp :: _ => Query.Match(snp,criteria)
        }
    ){
      crit =>
        val matcher =
          criteriaMatcher(crit)
              
        cache.values
          .collect { 
            case snp :: _ => Query.Match(snp,matcher(snp.data))
          }
          .filter(_.matchingCriteria.isDefined)
    }
    .toSeq
    .pure
    .map(_.asRight[String])

  }


  override def ?(
    patient: Id[Patient],
    timestamp: Option[Long] = None
  )(
    implicit env: C[F]
  ): F[Option[Snapshot[T]]] = 
    cache.get(patient)
      .flatMap {
        snps =>
          timestamp match {
            case None     => snps.headOption
            case Some(ts) => snps.find(_.timestamp == ts)
          }
      }
      .pure


  override def patientDataCounts(
    optCriteria: Option[Controlling.Criteria]
  )(
    implicit env: C[F]
  ): F[PatientDataCounts] =
    env.pure { 
      PatientDataCounts.from(
        cache.collect { case (_,history) if history.nonEmpty => history.head.data },
        optCriteria
      )
    }

}
