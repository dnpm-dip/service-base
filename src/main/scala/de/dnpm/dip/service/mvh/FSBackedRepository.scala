package de.dnpm.dip.service.mvh


import java.io.{
  File,
  FileInputStream,
  FileWriter,
  InputStream
}
import java.nio.file.Files
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import java.time.LocalDateTime
import scala.reflect.ClassTag
import scala.util.chaining._
import scala.util.{
  Failure,
  Success,
  Try,
  Using
}
import scala.collection.concurrent.{
  Map,
  TrieMap
}
import cats.Monad
import cats.data.{
  EitherNel,
  NonEmptyList
}
//import cats.syntax.apply._
import cats.syntax.applicative._
import cats.syntax.either._
import cats.syntax.functor._
import cats.syntax.traverse._
import play.api.libs.json.{
  Json,
  Reads,
  OFormat,
  Writes
}
import de.dnpm.dip.util.Logging
import de.dnpm.dip.model.{
  History,
  Id,
  Patient,
  PatientRecord,
  Period
}
import de.dnpm.dip.service.controlling.{
  Controlling,
  PatientDataCounts
}
import MVHService.DeletionEvent


// Extractor for TANs according to file naming pattern below
object TAN
{
  private val regex = ".+_TAN_([a-fA-F0-9]+)\\.json".r

  def unapply(filename: String): Option[Id[TransferTAN]] =
    regex.findFirstMatchIn(filename)
      .map(_.group(1))
      .map(Id[TransferTAN](_))

  def unapply(file: File): Option[Id[TransferTAN]] =
    unapply(file.getName)

}


// Extractor for Patient IDs according to file naming pattern below
object PatientId
{
  private val regex = ".+_Patient_(.+)_TAN_.+\\.json".r

  def unapply(filename: String): Option[Id[Patient]] =
    regex.findFirstMatchIn(filename)
      .map(_.group(1))
      .map(Id[Patient](_))

  def unapply(file: File): Option[Id[Patient]] =
    unapply(file.getName)

}


class FSBackedRepository[F[+_],T <: PatientRecord: OFormat](
  dataDir: File
)(
  implicit classTag: ClassTag[T]
)
extends Repository[F,Monad[F],T]
with Logging
{

  type Env = Monad[F]

  /**
   * Implicit conversion of Submission.Filter into a predicate function for PartialSubmission,
   * for filtering on cached PartialSubmissions
   */
  protected implicit def partialSubmissionPredicate(
    filter: Submission.Filter
  ): PartialSubmission => Boolean =
    submission =>
      filter.period.map(_ contains submission.submittedAt).getOrElse(true) &&
      filter.`type`.map(_ contains submission.metadata.`type`).getOrElse(true)


  private val tolerantSubmissionReads = Submission.tolerantReads.submission[T]


  private val REPORT_PREFIX = "SubmissionReport"

  private val SUBMISSION_PREFIX = s"MVH_${classTag.runtimeClass.getSimpleName}"

  private val DELETION_PREFIX = "DeletionEvent"


  private def reportFile(id: Id[Patient], tan: Id[TransferTAN]): File =
    new File(dataDir,s"${REPORT_PREFIX}_Patient_${id.value}_TAN_${tan.value}.json")

  private def submissionFile(id: Id[Patient], tan: Id[TransferTAN]): File =
    new File(dataDir,s"${SUBMISSION_PREFIX}_Patient_${id.value}_TAN_${tan.value}.json")

  private def reportFiles(id: Id[Patient]): Array[File] =
    dataDir.listFiles(
      (_,name) => name startsWith s"${REPORT_PREFIX}_Patient_${id.value}_TAN"
    ) 

  private def submissionFiles(id: Id[Patient]): Array[File] =
    dataDir.listFiles(
      (_,name) => name startsWith s"${SUBMISSION_PREFIX}_Patient_${id.value}_TAN"
    ) 

  private def reportFile(tan: Id[TransferTAN]): Option[File] =
    dataDir.listFiles(
      (_,name) => (name startsWith REPORT_PREFIX) && (name endsWith s"TAN_${tan.value}.json")
    )
    .toList
    .headOption

  private def submissionFile(tan: Id[TransferTAN]): Option[File] =
    dataDir.listFiles(
      (_,name) => (name startsWith SUBMISSION_PREFIX) && (name endsWith s"TAN_${tan.value}.json")
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


  private val cachedReports: Map[Id[TransferTAN],Submission.Report] =
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
  

  /**
   * Cache the PartialSubmissions for rapid in-memory filtering 
   */
  private val cachedPartialSubmissions: Map[Id[TransferTAN],PartialSubmission] =
    TrieMap.from(
      dataDir.listFiles(
        (_,name) => (name startsWith s"${SUBMISSION_PREFIX}_Patient") && (name endsWith ".json")
      )
      .to(LazyList)
      .map(new FileInputStream(_))
      .map(readAsJson[PartialSubmission])
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
        cachedPartialSubmissions += submission.metadata.transferTAN -> submission

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


  override def ?(filter: Submission.Filter)(
    implicit env: Env
  ): F[LazyList[Submission[T]]] = {

    val predicate: PartialSubmission => Boolean = filter

    val reader = readAsJson(tolerantSubmissionReads)

    cachedPartialSubmissions
      .collect { case (tan,sub) if predicate(sub) => submissionFile(sub.patient.id,tan) }
      .to(LazyList)
      .map(file => reader(new FileInputStream(file)))
      .pure
  }


  override def patientDataCounts(
    optCriteria: Option[Controlling.Criteria]
  )(
    implicit env: Env
  ): F[PatientDataCounts] =
    for {
      submissionsByPatient <- env.pure {
        cachedPartialSubmissions
          .values
          .groupBy(_.patient.id)
          .map { case (_,subs) => subs.maxBy(_.submittedAt) }
      }
      
      matchingCriteria =
        optCriteria.map(criteria =>
          submissionsByPatient.count(
            _.episodesOfCare.exists(eoc => criteria.episodeOfCarePeriod contains eoc.period.start)
          )
        )

    } yield PatientDataCounts(
      submissionsByPatient.size,
      matchingCriteria
    )
 

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


  private def save(
    event: DeletionEvent
  ): Either[String,DeletionEvent] =
    Using(new FileWriter(new File(dataDir,s"${DELETION_PREFIX}_TAN_${event.tan.value}.json"))){
      _.write(Json.stringify(Json.toJson(event)))
    }
    .fold(
      _ => s"Failed saving DeletionEvent ${Json.toJson(event)}".asLeft,
      _ => event.asRight
    )

  private def delete(
    event: DeletionEvent
  ): Either[String,DeletionEvent] =
    Try(new File(dataDir,s"${DELETION_PREFIX}_TAN_${event.tan.value}.json"))
      .map(file => Files.delete(file.toPath))
      .fold(
        _ => s"Failed removing DeletionEvent ${Json.toJson(event)}".asLeft,
        _ => event.asRight
      )

  override def deleteSubmission(tan: Id[TransferTAN])(
    implicit env: Env
  ): F[EitherNel[String,DeletionEvent]] =
    env.pure {
      (submissionFile(tan),reportFile(tan)) match {
      
        case (Some(subFile),Some(repFile)) =>
      
          val PatientId(patId) = subFile
          val backupSubmissionFile = Files.copy(subFile.toPath,Files.createTempFile(dataDir.toPath,s"REMOVED_Submission_$tan",".json"),REPLACE_EXISTING)
          val backupReportFile     = Files.copy(repFile.toPath,Files.createTempFile(dataDir.toPath,s"REMOVED_Report_$tan",".json"),REPLACE_EXISTING)

          val outcome = (Try{Files.delete(subFile.toPath)},Try{Files.delete(repFile.toPath)}) match {

            case (Success(_),Success(_)) =>

              val eventOutcome = save(DeletionEvent(patId,tan,LocalDateTime.now))

              val backupCleanup = Try {
                Files.delete(backupSubmissionFile)
                Files.delete(backupReportFile)
              }

              (backupCleanup,eventOutcome) match {
                case (Success(_),Right(_)) =>
                  cachedPartialSubmissions -= tan
                  cachedReports -= tan
                  eventOutcome

                case (Failure(_),Right(event)) =>
                  delete(event)

                case (_,failed) => failed
              } 
                
            case (Failure(t1),Success(_)) =>
              log.error(s"Failed to delete Submission $tan, restoring deleted SubmissionReport",t1)
              Try(Files.move(backupReportFile,repFile.toPath))
                .fold(
                  _ => s"Error occurred deleting Submission $tan".asLeft,
                  _ => s"Error occurred deleting Submission $tan".asLeft
                )

            case (Success(_),Failure(t2)) =>
              log.error(s"Failed to delete SubmissionReport $tan, restoring deleted Submission",t2)
              Try(Files.move(backupSubmissionFile,subFile.toPath))
                .fold(
                  _ => s"Error occurred deleting Submission $tan".asLeft,
                  _ => s"Error occurred deleting Submission $tan".asLeft
                )
         
            case (Failure(t1),Failure(t2)) =>
              log.error(s"Failed to delete Submission $tan",t1)
              log.error(s"Failed to delete SubmissionReport $tan",t2)
              Try {
                Files.move(backupSubmissionFile,subFile.toPath)
                Files.move(backupReportFile,repFile.toPath)
              }
              .fold(
                _ => s"Error occurred deleting Submission $tan".asLeft,
                _ => s"Error occurred deleting Submission $tan".asLeft
              )
          } 

          outcome.toEitherNel
         
        case _ =>
          s"Error deleting Submission data TAN $tan: Missing Submission or SubmissionReport file"
            .tap(log.error)
            .asLeft 
            .toEitherNel
      
      }
    }

/*
  override def deleteSubmission(tan: Id[TransferTAN])(
    implicit env: Env
  ): F[EitherNel[String,DeletionEvent]] =
    env.pure {
      (submissionFile(tan),reportFile(tan)) match {
      
        case (Some(subFile),Some(repFile)) =>
      
          val PatientId(patId) = subFile

          val backupSubmissionFile = Files.copy(subFile.toPath,Files.createTempFile(dataDir.toPath,s"REMOVED_Submission_$tan",".json"),REPLACE_EXISTING)
          val backupReportFile     = Files.copy(repFile.toPath,Files.createTempFile(dataDir.toPath,s"REMOVED_Report_$tan",".json"),REPLACE_EXISTING)
          
          val submissionDeletion = Try(subFile.toPath.tap(Files.delete)).recoverWith {
            case t => 
              log.error(s"Failed to delete Submission $tan",t)
              Try(Files.move(backupSubmissionFile,subFile.toPath))
          }

          val reportDeletion = Try(repFile.toPath.tap(Files.delete)).recoverWith {
            case t => 
              log.error(s"Failed to delete Submission $tan",t)
              Try(Files.move(backupReportFile,repFile.toPath))
          }

          (submissionDeletion,reportDeletion).mapN {
            (_,_) =>
              cachedPartialSubmissions -= tan
              cachedReports -= tan
              Files.delete(backupSubmissionFile)
              Files.delete(backupReportFile)
              save(DeletionEvent(patId,tan,LocalDateTime.now))
          }
          .getOrElse(s"Error(s) occurred deleting Submission $tan".asLeft)
          .toEitherNel
         
        case _ =>
          s"Error deleting Submission data TAN $tan: Missing Submission or SubmissionReport file"
            .tap(log.error)
            .asLeft 
            .toEitherNel
      
      }
    }
*/

  override def delete(patId: Id[Patient])(
    implicit env: Env
  ): F[EitherNel[String,List[DeletionEvent]]] = 
    for {
      subFiles <- submissionFiles(patId).pure

      outcomes = subFiles.foldLeft(
        List.empty[EitherNel[String,DeletionEvent]]
      ){ 
        (events,submissionFile) =>
      
          val TAN(tan) = submissionFile

          if (submissionFile.delete){
            cachedPartialSubmissions -= tan

            val repFile = reportFile(patId,tan)

            if (repFile.delete){
              cachedReports -= tan
              save(DeletionEvent(patId,tan,LocalDateTime.now)).toEitherNel :: events
            } else {
              log.error(s"Failed to delete $REPORT_PREFIX file $repFile")
              s"Failed to delete SubmissionReport for TAN $tan".asLeft.toEitherNel :: events
            }
          } else {
            log.error(s"Failed to delete $SUBMISSION_PREFIX file $submissionFile")
            s"Failed to delete Submission for TAN $tan".asLeft.toEitherNel :: events
          }
      }
      
    } yield outcomes.sequence


  override def deletionEvents(
    period: Period[LocalDateTime],
  )(
    implicit env: Env
  ): F[LazyList[DeletionEvent]] =
    dataDir.listFiles(
      (_,name) => (name startsWith DELETION_PREFIX) && (name endsWith ".json")
    )
    .to(LazyList)
    .map(new FileInputStream(_))
    .map(readAsJson[DeletionEvent])
    .filter(period)
    .pure

}
