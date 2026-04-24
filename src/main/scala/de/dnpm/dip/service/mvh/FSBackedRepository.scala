package de.dnpm.dip.service.mvh


import java.io.{
  File,
  FileInputStream,
  FileWriter,
  InputStream
}
import java.time.LocalDateTime
import scala.reflect.ClassTag
import scala.util.chaining._
import scala.util.Using
import scala.collection.concurrent.{
  Map,
  TrieMap
}
import cats.Monad
import cats.data.NonEmptyList
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.applicative._
import cats.syntax.either._
import play.api.libs.json.{
  Json,
  JsPath,
  Reads,
  Writes,
  OWrites
}
import de.dnpm.dip.util.Logging
import de.dnpm.dip.model.{
  History,
  Id,
  Patient,
  PatientRecord,
}


// Extractor for TANs according to file naming pattern below
object TAN
{

  private val regex = ".+_TAN_([a-fA-F0-9]+).json".r

  def unapply(filename: String): Option[Id[TransferTAN]] =
    regex.findFirstMatchIn(filename)
      .map(_.group(1))
      .map(Id[TransferTAN](_))

  def unapply(file: File): Option[Id[TransferTAN]] =
    unapply(file.getName)

}

/**
 * Light-weight "header" version of a Submission,
 * containing only the necessary data elements to perform filtering by Submission.Filter,
 */
private[mvh] final case class SubmissionHeader
(
  metadata: Submission.Metadata,
  patient: Patient,
  submittedAt: LocalDateTime
)

private[mvh] object SubmissionHeader
{
  import play.api.libs.functional.syntax._

  implicit def fromSubmission[T <: PatientRecord](sub: Submission[T]): SubmissionHeader =
    SubmissionHeader(
      sub.metadata,
      sub.record.patient,
      sub.submittedAt
    )

  // Explicit Reads to include tolerant Reads[Submission.Metadata],
  // required for backwards compatibility with data sets potentially containing 
  // strcutrally invalid Consent resources
  implicit val reads: Reads[SubmissionHeader] =
    (
      (JsPath \ "metadata").read(Submission.tolerantReads.metadata) and
      (JsPath \ "patient").read[Patient] and
      (JsPath \ "submittedAt").read[LocalDateTime]
    )(
      SubmissionHeader(_,_,_)
    )

}


class FSBackedRepository[F[_],T <: PatientRecord: Reads: OWrites](
  dataDir: File
)(
  implicit classTag: ClassTag[T]
)
extends Repository[F,Monad[F],T]
with Logging
{

  type Env = Monad[F]

  /**
   * Implicit conversion of Submission.Filter into a predicate function for SubmissionHeader,
   * for filtering on cached SubmissionHeaders
   */
  protected implicit def submissionHeaderPredicate(
    filter: Submission.Filter
  ): SubmissionHeader => Boolean =
    submission =>
      filter.period.map(_ contains submission.submittedAt).getOrElse(true) &&
      filter.`type`.map(_ contains submission.metadata.`type`).getOrElse(true)


  private val tolerantSubmissionReads = Submission.tolerantReads.submission[T]


  private val REPORT_PREFIX = "SubmissionReport"

  private val SUBMISSION_PREFIX = s"MVH_${classTag.runtimeClass.getSimpleName}"

  private def reportFile(id: Id[Patient], tan: Id[TransferTAN]): File =
    new File(dataDir,s"${REPORT_PREFIX}_Patient_${id.value}_TAN_${tan.value}.json")

  private def submissionFile(id: Id[Patient], tan: Id[TransferTAN]): File =
    new File(dataDir,s"${SUBMISSION_PREFIX}_Patient_${id.value}_TAN_${tan.value}.json")

  private def reportFiles(id: Id[Patient]): Array[File] =
    dataDir.listFiles(
      (_,name) => name startsWith s"${REPORT_PREFIX}_Patient_${id.value}"
    ) 

  private def submissionFiles(id: Id[Patient]): Array[File] =
    dataDir.listFiles(
      (_,name) => name startsWith s"${SUBMISSION_PREFIX}_Patient_${id.value}"
    ) 

  private def submissionFile(tan: Id[TransferTAN]): Option[File] =
    dataDir.listFiles(
      (_,name) => (name startsWith s"${SUBMISSION_PREFIX}") && (name endsWith s"TAN_${tan.value}.json")
    )
    .toList
    .headOption

  private def toPrettyJson[A: Writes](a: A): String =
    Json.toJson(a) pipe Json.prettyPrint

  private def readAsJson[A](implicit reads: Reads[A]): InputStream => A =
    in =>
      Json.parse(in)
        .pipe(Json.fromJson[A](_))
        .pipe(_.get)
        .tap(_ => in.close)


  private val cachedReports: Map[Id[TransferTAN],Submission.Report] = {

    // Migrate persisted Submissions and SubmissionReports from the previous to the new file naming pattern
    // TODO: Remove when ensured all sites have successfully migrated
    @annotation.unused val oldFileMigration = {
      val failedReportMigrations =
        dataDir.listFiles(
          (_,name) =>
            (name startsWith REPORT_PREFIX) &&
            !(name contains "_Patient_") && !(name contains "_TAN_") &&
            (name endsWith ".json")
        )
        .foldLeft(List.empty[String]){
          (acc,src) =>
            val report = new FileInputStream(src) pipe readAsJson[Submission.Report]
            val target = reportFile(report.patient,report.id)
        
            if (!src.renameTo(target)) s"Failed to migrate/rename $REPORT_PREFIX file $src to $target, consider performing this manually" :: acc
            else acc
        }
   
      val failedMigrations =
        dataDir.listFiles(
          (_,name) =>
            (name startsWith SUBMISSION_PREFIX) &&
            !(name contains "_Patient_") && !(name contains "_TAN_") &&
            (name endsWith ".json")
        )
        .foldLeft(failedReportMigrations){
          (acc,src) =>
            val submission = new FileInputStream(src) pipe readAsJson(tolerantSubmissionReads)
            val target = submissionFile(submission.record.id,submission.metadata.transferTAN)

            if (!src.renameTo(target)) s"Failed to migrate/rename $SUBMISSION_PREFIX file $src to $target, consider performing this manually" :: acc
            else acc
        }
      
      if (failedMigrations.nonEmpty)
        failedMigrations.tapEach(msg => log.error(s"FATAL - $msg"))
          .mkString("\n")
          .pipe(new RuntimeException(_))
    }
    // End of migration code

    TrieMap.from(
      dataDir.listFiles(
        (_,name) =>
          (name startsWith REPORT_PREFIX) &&
          (name contains "_Patient_") &&
          (name contains "_TAN_") &&
          (name endsWith ".json")
      )
      .to(Iterable)
      .map(new FileInputStream(_))
      .map(readAsJson[Submission.Report])
      .map(r => r.id -> r)
    )
  }

  /**
   * Cache the SubmissionHeaders for rapid in-memory filtering 
   */
  private val cachedSubmissionHeaders: Map[Id[TransferTAN],SubmissionHeader] =
    TrieMap.from(
      dataDir.listFiles(
        (_,name) => (name startsWith s"${SUBMISSION_PREFIX}_Patient") && (name endsWith ".json")
      )
      .to(Iterable)
      .map(new FileInputStream(_))
      .map(readAsJson[SubmissionHeader])
      .map(sub => sub.metadata.transferTAN -> sub)
    )



  override def alreadyUsed(id: Id[TransferTAN])(
    implicit env: Env
  ): F[Boolean] =
    cachedReports.contains(id).pure


  override def save(
    report: Submission.Report,
    submission: Submission[T]
  )(
    implicit env: Env
  ): F[Either[String,Unit]] =
    Using.resources(
      new FileWriter(reportFile(submission.record.id,submission.metadata.transferTAN)),
      new FileWriter(submissionFile(submission.record.id,submission.metadata.transferTAN))
    ){
      (repWriter,subWriter) =>
        repWriter.write(toPrettyJson(report))
        subWriter.write(toPrettyJson(submission))

        cachedReports += report.id -> report
        cachedSubmissionHeaders += submission.metadata.transferTAN -> submission

        ().asRight[String]
    }
    .pure


  override def submissionReport(
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
    Using(new FileWriter(reportFile(report.patient,report.id))){
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
  ): F[Seq[Submission[T]]] = {

    val filter: SubmissionHeader => Boolean = fltr

    cachedSubmissionHeaders
      .collect {
        case (tan,sub) if filter(sub) => 
          submissionFile(sub.patient.id,tan)
            .pipe(new FileInputStream(_))
            .pipe(readAsJson(tolerantSubmissionReads))
      }
      .toSeq
      .pure
  }


  override def submission(
    id: Id[TransferTAN]
  )(
    implicit env: Env
  ): F[Option[Submission[T]]] =
    submissionFile(id)
      .map(new FileInputStream(_))
      .map(readAsJson(tolerantSubmissionReads))
      .pure


  override def submissionHistory(id: Id[Patient])(
    implicit env: Env
  ): F[Option[History[Submission[T]]]] =
    for {
      files <- submissionFiles(id).pure

      submissions =
        files
          .map(new FileInputStream(_))
          .map(readAsJson(tolerantSubmissionReads))

      history =
        NonEmptyList.fromList(submissions.toList)
          .map(History(_))

    } yield history


  override def submissionReportHistory(id: Id[Patient])(
    implicit env: Env
  ): F[Option[History[Submission.Report]]] =
    for {
      files <- reportFiles(id).pure

      reports =
        files
          .map(new FileInputStream(_))
          .map(readAsJson[Submission.Report])

      history =
        NonEmptyList.fromList(reports.toList)
          .map(History(_))

    } yield history


  override def delete(id: Id[Patient])(
    implicit env: Env
  ): F[Either[String,Unit]] =
    for {
      repFiles <- reportFiles(id).pure
      subFiles <- submissionFiles(id).pure

      submissionDeletionErrors =
        subFiles.foldLeft(List.empty[String]){
          (acc,file) => 
            if (file.delete) acc
            else s"Failed to delete $SUBMISSION_PREFIX file $file" :: acc
        }

      deletionErrors =
        repFiles.foldLeft(submissionDeletionErrors){
          (acc,file) =>
        
            val TAN(tan) = file
        
            if (file.delete){
              cachedReports -= tan 
              acc
            }
            else s"Failed to delete $REPORT_PREFIX file $file" :: acc
        }

      result =
        if (deletionErrors.isEmpty) ().asRight
        else deletionErrors.tapEach(log.error).mkString("; ").asLeft

    } yield result
     
}

