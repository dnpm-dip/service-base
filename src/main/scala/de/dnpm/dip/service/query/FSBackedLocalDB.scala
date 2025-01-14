package de.dnpm.dip.service.query


import java.io.{
  File,
  FileWriter,
  FileInputStream
}
import scala.reflect.ClassTag
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
  Snapshot
}
import QueryService.{
  Saved,
  Deleted
}
import play.api.libs.json.{
  Json,
  Format,
  Reads
}



class FSBackedLocalDB[
  F[_],
  C[M[_]] <: Applicative[M],
  Criteria,
  PatientRecord <: { val patient: Patient } : Format
](
  val dataDir: File,
  val criteriaMatcher: Criteria => (PatientRecord => Option[Criteria]),
)(
  implicit classTag: ClassTag[PatientRecord]
)
extends LocalDB[F,C[F],Criteria,PatientRecord]
with Logging
{

  import scala.language.reflectiveCalls
  import scala.util.Using
  import scala.util.chaining._
  import cats.syntax.functor._
  import cats.syntax.applicative._
  import cats.syntax.either._

  private val prefix =
    classTag.runtimeClass.getSimpleName


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
    new File(dataDir,fileName(snp.data.patient.id,snp.timestamp))


  private def readJson[T: Reads](file: File): T =
    Json.parse(new FileInputStream(file))
      .pipe(Json.fromJson[T](_))
      .tap(
        _.fold(
          errs => log.error(s"Error(s) occurred parsing file $file: \n${errs.toString}"),
          _ => ()
        )
      )
      .pipe(_.get)


  private val cache: Map[Id[Patient],Snapshot[PatientRecord]] = {

    log.debug(s"Setting up file-system persistence to directory ${dataDir.getAbsolutePath}")

    if (!dataDir.exists)
      dataDir.mkdirs
        .tap {
          case false =>
            log.warn(
              s"Failed to create directory ${dataDir.getAbsolutePath}. Ensure the executing user has appropriate permissions on the directory."
            )
          case _ => ()
        }
    
    dataDir.listFiles(
      (_,name) => (name startsWith prefix) && (name endsWith ".json")
    )
    .to(LazyList)
    .map(readJson[Snapshot[PatientRecord]])
    // Lazily accumulate only the latest snapshot of each patient record,
    // instead of using groupyBy(patientId) and then picking the latest snapshot,
    // which requires all to be loaded into memory, whereas with this 
    // implementation, elements can be garbage-collected along the way
    .foldLeft(TrieMap.empty[Id[Patient],Snapshot[PatientRecord]]){ 
      (acc,snp) =>
        acc.updateWith(snp.data.patient.id){
          case Some(s) =>
            if (snp.timestamp > s.timestamp) Some(snp)
            else Some(s)

          case None => Some(snp)
        }
        acc
    }
    
  }


  override def save(
    dataSet: PatientRecord
  )(
    implicit env: C[F]
  ): F[Either[String,Saved[PatientRecord]]] = {
  
    //TODO: Logging
   
    val snp = Snapshot.of(dataSet)

    Using(new FileWriter(fileOf(snp))){
      _.write(
        Json.toJson(snp) pipe Json.stringify
      )
    }
    .map(_ => cache update (dataSet.patient.id,snp))
    .fold(
      _.getMessage.asLeft[Saved[PatientRecord]],
      _ => Saved(snp).asRight[String]
    )
    .pure

  }


  override def delete(
    patId: Id[Patient],
  )(
    implicit env: C[F]
  ): F[Either[String,Deleted]] = {

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
          .map(_ => cache -= patId)
          .recoverWith {
            case t =>
              log.error(s"Couldn't delete file ${file.getAbsolutePath}",t)
              Failure(t)
          }

    )
    .sequence
    .fold(
      _ => s"Error(s) occurred deleting data files of Patient ${patId.value}, check the log".asLeft[Deleted],
      _ => Deleted(patId).asRight[String] 
    )
    .pure

  }


  override def ?(
    criteria: Option[Criteria]
  )(
    implicit env: C[F]
  ): F[Either[String,Seq[Query.Match[PatientRecord,Criteria]]]] = {

    criteria.fold(
      cache.values
        .map(Query.Match(_,criteria))

    ){
      crit =>

        val matcher =
          criteriaMatcher(crit)
              
        cache.values
          .map(snp => Query.Match(snp,matcher(snp.data)))
          .filter(_.matchingCriteria.isDefined)
    }
    .toSeq
    .pure
    .map(_.asRight[String])

  }


  override def ?(
    patient: Id[Patient],
    snapshot: Option[Long] = None
  )(
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
          .map(readJson[Snapshot[PatientRecord]])

        case None =>
          cache.get(patient)
      
      }
    )
    .pure

  }

}


