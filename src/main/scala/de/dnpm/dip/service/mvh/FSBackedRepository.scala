package de.dnpm.dip.service.mvh


import java.io.{
  File,
  FileInputStream,
  FileWriter,
  InputStream
}
import scala.reflect.ClassTag
import scala.util.chaining._
import scala.util.Using
import scala.collection.concurrent.{
  Map,
  TrieMap
}
import cats.Monad
import cats.syntax.functor._
import cats.syntax.applicative._
import cats.syntax.either._
import play.api.libs.json.{
  Json,
  Reads,
  Writes,
  OWrites
}
import de.dnpm.dip.model.{
  Id,
  Patient,
  PatientRecord,
}


class FSBackedRepository[F[_],T <: PatientRecord: Reads: OWrites](
  dataDir: File
)(
  implicit classTag: ClassTag[T]
)
extends Repository[F,Monad[F],T]
{

  type Env = Monad[F]

  private val REPORT_PREFIX = "SubmissionReport_"

  private val SUBMISSION_PREFIX = s"MVH_${classTag.runtimeClass.getSimpleName}_"


  private val cachedReports: Map[Id[TransferTAN],Submission.Report] =
    TrieMap.from(
      dataDir.listFiles(
        (_,name) => (name startsWith REPORT_PREFIX) && (name endsWith ".json")
      )
      .to(Iterable)
      .map(new FileInputStream(_))
      .map(readAsJson[Submission.Report])
      .map(r => r.id -> r)
    )


  private def reportFile(id: Id[Patient]): File =
    new File(dataDir,s"$REPORT_PREFIX${id.value}.json")

  private def submissionFile(id: Id[Patient]): File =
    new File(dataDir,s"$SUBMISSION_PREFIX${id.value}.json")


  private def toPrettyJson[A: Writes](a: A): String =
    Json.toJson(a) pipe Json.prettyPrint

  private def readAsJson[A: Reads]: InputStream => A =
    Json.parse(_)
      .pipe(Json.fromJson[A](_))
      .pipe(_.get)



  override def save(
    report: Submission.Report,
    submission: Submission[T]
  )(
    implicit env: Env
  ): F[Either[String,Unit]] =
    Using.resources(
      new FileWriter(reportFile(submission.record.id)),
      new FileWriter(submissionFile(submission.record.id))
    ){
      (repWriter,subWriter) =>
        repWriter.write(toPrettyJson(report))
        subWriter.write(toPrettyJson(submission))

        cachedReports += report.id -> report

        ().asRight[String]
    }
    .pure


  override def ?(
    id: Id[TransferTAN]
  )(
    implicit env: Env
  ): F[Option[Submission.Report]] =
    cachedReports
      .get(id)
      .pure


  override def update(
    report: Submission.Report,
  )(
    implicit env: Env
  ): F[Either[String,Unit]] =
    Using(new FileWriter(reportFile(report.patient))){
      w =>
        w.write(toPrettyJson(report))
        cachedReports.update(report.id,report)
    }
    .fold(
      _ => s"Update failed on Submission.Report ${report.id}".asLeft,
      _ => ().asRight
    )
    .pure


  override def ?(fltr: Submission.Report.Filter)(
    implicit env: Env
  ): F[Seq[Submission.Report]] =
    cachedReports
      .values
      .filter(fltr)
      .toSeq
      .pure


  override def ?(fltr: Submission.Filter)(
    implicit env: Env
  ): F[Seq[Submission[T]]] =
    dataDir.listFiles(
      (_,name) => (name startsWith SUBMISSION_PREFIX) && (name endsWith ".json")
    )
    .to(Seq)
    .map(new FileInputStream(_))
    .map(readAsJson[Submission[T]])
    .filter(fltr)
    .pure


  override def delete(id: Id[Patient])(
    implicit env: Env
  ): F[Either[String,Unit]] =
    for {
      repFile <- reportFile(id).pure
      report = readAsJson[Submission.Report].apply(new FileInputStream(repFile))
    } yield {
      if (submissionFile(id).delete && repFile.delete){
        cachedReports -= report.id
        ().asRight[String]
      }
      else
        s"Failed to delete Submission for Patient $id".asLeft[Unit]
    }

}
