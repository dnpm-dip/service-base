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
import cats.Monad
import cats.syntax.applicative._
import cats.syntax.either._
import play.api.libs.json.{
  Json,
  Format,
  Reads,
  Writes
}
import de.dnpm.dip.model.{
  Id,
  Patient,
  PatientRecord,
}


private[mvh] final case class SubmissionReportWithId
(
  patient: Id[Patient],
  report: SubmissionReport
)

object SubmissionReportWithId
{
  private[mvh] implicit val format: Format[SubmissionReportWithId] =
    Json.format[SubmissionReportWithId]
}


class FSBackedRepository[F[_],T <: PatientRecord: Reads: Writes](
  dataDir: File
)(
  implicit classTag: ClassTag[T]
)
extends Repository[F,Monad[F],T]
{

  type Env = Monad[F]


  private def mvhRecordFile(id: Id[Patient]): File =
    new File(dataDir,s"MVH_${classTag.runtimeClass.getSimpleName}_${id.value}.json")

  private def reportFile(id: Id[Patient]): File =
    new File(dataDir,s"SubmissionReport_${id.value}.json")


  private def toPrettyJson[T: Writes](t: T): String =
    Json.toJson(t) pipe Json.prettyPrint

  private def readAsJson[T: Reads]: InputStream => T =
    Json.parse(_)
      .pipe(Json.fromJson[T](_))
      .pipe(_.get)



  override def save(
    mvhRecord: MVHPatientRecord[T],
    report: SubmissionReport
  )(
    implicit env: Env
  ): F[Either[String,Unit]] = {
    val patId = mvhRecord.record.id

    Using.resources(
      new FileWriter(mvhRecordFile(patId)),
      new FileWriter(reportFile(patId))
    ){
      (recordWriter,reportWriter) =>
        recordWriter.write(toPrettyJson(mvhRecord))
        reportWriter.write(
          toPrettyJson(
            SubmissionReportWithId(patId,report)
          )
        )
    }
    .asRight[String]
    .pure
  }


  override def ?(fltr: SubmissionReport.Filter)(
    implicit env: Env
  ): F[Iterable[SubmissionReport]] =
    dataDir.listFiles(
      (_,name) => (name startsWith "SubmissionReport") && (name endsWith ".json")
    )
    .to(Iterable)
    .map(new FileInputStream(_))
    .map(readAsJson[SubmissionReport])
    .filter(fltr)
    .pure


  override def delete(id: Id[Patient])(
    implicit env: Env
  ): F[Either[String,Unit]] = 
    (mvhRecordFile(id).delete && reportFile(id).delete) match {
      case true  =>
        ().asRight[String].pure

      case false =>
        s"Failed to delete PatientRecord and/or SubmissionReport for Patient $id".asLeft[Unit].pure
    }

}
