package de.dnpm.dip.service.mvh


import de.dnpm.dip.service.Distribution
import de.dnpm.dip.model.{
  Id,
  Patient,
  PatientRecord,
}
import play.api.libs.json.{ 
  Json,
  OFormat
}

trait MVHService[F[_],Env,T <: PatientRecord]
{
  import MVHService._

  type ReportType <: Report

  val useCase: UseCase.Value

  def !(cmd: Command[T])(
    implicit env: Env
  ): F[Either[Error,Outcome]]


  def ?(filter: Submission.Report.Filter)(
    implicit env: Env
  ): F[Seq[Submission.Report]]


  def ?(
    id: Id[TransferTAN]
  )(
    implicit env: Env
  ): F[Option[Submission.Report]]


  def ?(filter: Submission.Filter)(
    implicit env: Env
  ): F[Seq[Submission[T]]]
  
  
  def statusInfo(
    implicit env: Env
  ): F[StatusInfo]

  
  def report(
    criteria: Report.Criteria
  )(
    implicit env: Env
  ): F[ReportType]

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
  final case class GenericError(msg: String) extends Error


  final case class StatusInfo
  (
    submissionReports: Distribution[Submission.Report.Status.Value]
  )

  object StatusInfo
  {
    implicit val format: OFormat[StatusInfo] =
      Json.format[StatusInfo]
  }

}
