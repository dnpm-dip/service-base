package de.dnpm.dip.service.mvh


import java.time.LocalDateTime
import cats.data.EitherNel
import de.dnpm.dip.model.{
  History,
  Id,
  Patient,
  PatientRecord,
  Period
}
import MVHService.DeletionEvent
import de.dnpm.dip.service.controlling.Controlling


trait Repository[F[_],Env,T <: PatientRecord] extends Controlling.Ops[F,Env]
{

  protected implicit def submissionReportPredicate(filter: Submission.Report.Filter): Submission.Report => Boolean =
    report =>
      filter.status.map(_ contains report.status).getOrElse(true) &&
      filter.period.map(_ contains report.createdAt).getOrElse(true) &&
      filter.`type`.map(_ contains report.`type`).getOrElse(true) &&
      filter.patient.map(_ contains report.patient).getOrElse(true)


  protected implicit def submissionPredicate(filter: Submission.Filter): Submission[T] => Boolean =
    submission =>
      filter.period.map(_ contains submission.submittedAt).getOrElse(true) &&
      filter.`type`.map(_ contains submission.metadata.`type`).getOrElse(true) 


  protected implicit def deletionEventPredicate(period: Period[LocalDateTime]): DeletionEvent => Boolean =
    event => period contains event.dateTime
  
  
  def alreadyUsed(id: Id[TransferTAN])(
    implicit env: Env
  ): F[Boolean]

  def save(
    report: Submission.Report,
    record: Submission[T]
  )(
    implicit env: Env
  ): F[Either[String,Unit]]


  def update(
    report: Submission.Report,
  )(
    implicit env: Env
  ): F[Either[String,Unit]]


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


  def submissionHistory(id: Id[Patient])(
    implicit env: Env
  ): F[Option[History[Submission[T]]]]


  def submissionReportHistory(id: Id[Patient])(
    implicit env: Env
  ): F[Option[History[Submission.Report]]]

  
  def delete(id: Id[Patient])(
    implicit env: Env
  ): F[EitherNel[String,Seq[DeletionEvent]]]


  def deletionEvents(
    period: Period[LocalDateTime],
  )(
    implicit env: Env
  ): F[Seq[DeletionEvent]]

}
