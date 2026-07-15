package de.dnpm.dip.service.mvh


import java.time.LocalDateTime
import cats.data.EitherNel
import de.dnpm.dip.service.controlling.Controlling
import de.dnpm.dip.model.{
  Id,
  Patient,
  PatientRecord,
  Period
}
import play.api.libs.json.{
  Json,
  OFormat
}


trait MVHService[F[_],Env,T <: PatientRecord] extends Controlling.Ops[F,Env]
{
  import MVHService.{
    Command,
    DeletionEvent,
    Error,
    Outcome
  }

  type ReportType <: Report

  val useCase: UseCase.Value

  /**
   * Process a command, hence exclamation mark operator
   *
   * @param cmd The Command to be processed
   */
  def !(cmd: Command[T])(
    implicit env: Env
  ): F[EitherNel[Error,Outcome]]


  def ?(filter: Submission.Report.Filter)(
    implicit env: Env
  ): F[Seq[Submission.Report]]


  def submissionReport(
    id: Id[TransferTAN]
  )(
    implicit env: Env
  ): F[Option[Submission.Report]]


  def ?(filter: Submission.Filter)(
    implicit env: Env
  ): F[Seq[Submission[T]]]
  
  
  def submission(
    id: Id[TransferTAN]
  )(
    implicit env: Env
  ): F[Option[Submission[T]]]

  
  def report(
    criteria: Report.Criteria
  )(
    implicit env: Env
  ): F[ReportType]


  def deletionEvents(
    period: Period[LocalDateTime],
  )(
    implicit env: Env
  ): F[Seq[DeletionEvent]]

}


object MVHService
{
  sealed trait Command[+T]

  final case class Process[T <: PatientRecord](
    record: T,
    metadata: Submission.Metadata
  )
  extends Command[T]

  final case class ConfirmSubmitted(id: Id[TransferTAN]) extends Command[Nothing]
  final case class Delete(id: Id[Patient]) extends Command[Nothing]


  sealed trait Outcome
  final case object Saved extends Outcome
  final case object Updated extends Outcome
  final case object Deleted extends Outcome


  sealed trait Error
  final case class InvalidTAN(msg: String) extends Error
  final case class InvalidSubmissionType(msg: String) extends Error
  final case class GenericError(msg: String) extends Error



  final case class DeletionEvent
  (
    patient: Id[Patient],
    tan: Id[TransferTAN],
    dateTime: LocalDateTime
  )

  implicit val formatDeletionEvent: OFormat[DeletionEvent] =
    Json.format[DeletionEvent]

}
