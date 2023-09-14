package de.dnpm.dip.service.query


import java.io.{
  File,
  FileWriter,
  InputStream,
  FileInputStream
}
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
import play.api.libs.json.{
  Json,
  Format
}



class FSBackedLocalDB[
  F[_],
  C[M[_]] <: Applicative[M],
  Criteria,
  PatientRecord <: { val patient: Patient } : Format
](
  val dataDir: File,
  val prefix: String,
  val criteriaMatcher: Criteria => (PatientRecord => Criteria),
  val isEmpty: Criteria => Boolean
)
extends LocalDB[
  F,
  C[F],
//  Applicative[F],
  Criteria,
  PatientRecord
]
with Logging
{

  import scala.language.reflectiveCalls
  import scala.util.Using
  import scala.util.chaining._
  import cats.syntax.functor._
  import cats.syntax.applicative._
  import cats.syntax.either._


  private def inputStream(
    f: File
  ): InputStream =
    new FileInputStream(f)

  private def fileStart(
    patId: Id[Patient]
  ): String =
    s"${prefix}_${patId.value}"

  private def fileName(
    patId: Id[Patient],
    snpId: Long
  ): String =
    s"${fileStart(patId)}_Snapshot_${snpId}.json"

  private def fileOf(
    snp: Snapshot[PatientRecord]
  ): File =
    new File(dataDir,fileName(snp.data.patient.id,snp.id))



  private val cache: Map[Id[Patient],Snapshot[PatientRecord]] = {

    if (!dataDir.exists) dataDir.mkdirs

    TrieMap.from(
      dataDir.listFiles(
        (_,name) => (name startsWith prefix) && (name endsWith ".json")
      )
      .to(LazyList)
      .map(inputStream)
      .map(Json.parse)
      .map(Json.fromJson[Snapshot[PatientRecord]](_))
      .map(_.get)
      .groupBy(_.data.patient.id)
      // get most recent snapshot for caching
      .map {
        case (patId,snps) => patId -> snps.maxBy(_.id)
      }
    )

  }


  override def save(
    dataSet: PatientRecord
  )(
    implicit env: C[F]
//    implicit env: Applicative[F]
  ): F[Either[String,Data.Saved[PatientRecord]]] = {
  
    //TODO: Logging
   
    val snp = Snapshot(dataSet)

    Using(new FileWriter(fileOf(snp))){
      _.write(
        Json.toJson(snp) pipe Json.stringify
      )
    }
    .map(
      _ => cache.update(dataSet.patient.id,snp)
    )
    .fold(
      _.getMessage.asLeft[Data.Saved[PatientRecord]],
      _ => Data.Saved(snp).asRight[String]
    )
    .pure

  }


  override def delete(
    patId: Id[Patient],
  )(
//    implicit env: Applicative[F]
    implicit env: C[F]
  ): F[Either[String,Data.Deleted]] = {

    import java.nio.file.Files
    import cats.syntax.traverse._

    log.info(s"Deleting all patient record snapshot files for Patient ${patId.value}")

    dataDir.listFiles(
      (_,name) => name startsWith fileStart(patId)
    )
    .to(LazyList)
    .map(
      file =>
        Try(Files.delete(file.toPath))
          .recoverWith {
            case t =>
              log.error(s"Couldn't delete file ${file.getAbsolutePath}",t)
              Failure(t)
          }
    )
    .sequence
    .fold(
      _ => s"Error(s) occurred deleting data files of Patient ${patId.value}, check the log!".asLeft[Data.Deleted],
      _ => Data.Deleted(patId).asRight[String] 
    )
    .pure[F]

  }


  override def ?(
    criteria: Criteria
  )(
//    implicit env: Applicative[F]
    implicit env: C[F]
  ): F[Either[String,Seq[(Snapshot[PatientRecord],Criteria)]]] =

    (
      isEmpty(criteria) match {
        case false =>
          val matcher = criteriaMatcher(criteria)
          
          cache.values
            .map(
              snp => snp -> matcher(snp.data)
            )
            .filterNot {
              case (_,matches) => isEmpty(matches) 
            }
            .toSeq
            
        case true =>
          cache.values
            .map(_ -> criteria)
            .toSeq
  
      }
    )
    .pure
    .map(_.asRight[String])


  override def ?(
    patient: Id[Patient],
    snapshot: Option[Long] = None
  )(
//    implicit env: Applicative[F]
    implicit env: C[F]
  ): F[Option[Snapshot[PatientRecord]]] = {

    //TODO: Logging

    (
      snapshot match {
      
        case Some(snpId) =>
          dataDir.listFiles(
            (_,name) => name == fileName(patient,snpId)
          )
          .headOption
          .map(inputStream)
          .map(Json.parse)
          .map(Json.fromJson[Snapshot[PatientRecord]](_))
          .map(_.get)
      
        case None =>
          cache.get(patient)
      
      }
    )
    .pure

  }

}


