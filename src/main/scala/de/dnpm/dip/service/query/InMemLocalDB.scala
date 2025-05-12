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
  Snapshot
}
import QueryService.{
  Saved,
  Deleted
}


class InMemLocalDB[
  F[_],
  C[M[_]] <: Applicative[M],
  Criteria,
  PatientRecord <: { val patient: Patient }
](
  val criteriaMatcher: Criteria => (PatientRecord => Option[Criteria]),
)
extends LocalDB[
  F,
  C[F],
  Criteria,
  PatientRecord
]
with Logging
{

  import scala.language.reflectiveCalls
  import cats.syntax.functor._
  import cats.syntax.applicative._
  import cats.syntax.either._


  private val cache: Map[Id[Patient],List[Snapshot[PatientRecord]]] =
    TrieMap.empty



  override def save(
    dataSet: PatientRecord
  )(
    implicit env: C[F]
  ): F[Either[String,Saved[PatientRecord]]] = {
  
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
  ): F[Either[String,Seq[Query.Match[PatientRecord,Criteria]]]] = {

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
  ): F[Option[Snapshot[PatientRecord]]] = 
    cache.get(patient)
      .flatMap {
        snps =>
          timestamp match {
            case None     => snps.headOption
            case Some(ts) => snps.find(_.timestamp == ts)
          }
      }
      .pure


   def totalRecords(
    implicit env: C[F]
  ): F[Int] =
    cache.size.pure

}
