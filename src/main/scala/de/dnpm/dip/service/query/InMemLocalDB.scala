package de.dnpm.dip.service.query


import scala.util.{
  Either,
  Try,
  Failure,
}
import scala.collection.concurrent.{
  Map,
  TrieMap
}
import cats.Applicative
import de.dnpm.dip.util.Logging
import de.dnpm.dip.model.{
  Id,
  Patient,
  Snapshot,
  Site
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
  ): F[Either[String,Data.Saved[PatientRecord]]] = {
  
    //TODO: Logging
   
    val snp = Snapshot(dataSet)

    cache.updateWith(dataSet.patient.id){
      case Some(snps) => Some(snp :: snps)
      case None       => Some(List(snp))
    }

    Data.Saved(snp)
      .asRight[String]
      .pure

  }


  override def delete(
    patId: Id[Patient],
  )(
    implicit env: C[F]
  ): F[Either[String,Data.Deleted]] = {

    import java.nio.file.Files
    import cats.syntax.traverse._

    log.info(s"Deleting all patient record snapshots for Patient ${patId.value}")

    cache -= patId

    Data.Deleted(patId)
      .asRight[String]
      .pure[F]

  }


  override def ?(
    criteria: Criteria
  )(
    implicit env: C[F]
  ): F[Either[String,Seq[(Snapshot[PatientRecord],Criteria)]]] = {

    val matcher = criteriaMatcher(criteria)
          
    cache.values
      .collect { 
        case snp :: _ => snp -> matcher(snp.data)
      }
      .collect {
        case (snp,Some(matches)) => snp -> matches
      }
      .toSeq
      .pure
      .map(_.asRight[String])

  }


  override def ?(
    patient: Id[Patient],
    snapshotId: Option[Long] = None
  )(
    implicit env: C[F]
  ): F[Option[Snapshot[PatientRecord]]] = {

    //TODO: Logging

    cache.get(patient)
      .flatMap {
        snps =>
          snapshotId match {
            case None     => snps.headOption
            case Some(id) => snps.find(_.id == id)
          }
      }
      .pure

  }

}


