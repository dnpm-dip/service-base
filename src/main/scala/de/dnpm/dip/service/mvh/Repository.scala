package de.dnpm.dip.service.mvh


import de.dnpm.dip.model.{
  History,
  Id,
  Patient,
  PatientRecord,
}


trait Repository[F[_],Env,T <: PatientRecord]
{
/*
  type FilterableSubmission = { 
    def submittedAt: LocalDateTime
    def metadata: Submission.Metadata
  }

  import scala.language.reflectiveCalls

  protected implicit def submissionPredicate(filter: Submission.Filter): FilterableSubmission => Boolean =
*/

  protected implicit def submissionReportPredicate(filter: Submission.Report.Filter): Submission.Report => Boolean =
    report =>
      filter.status.map(_ contains report.status).getOrElse(true) &&
      filter.period.map(_ contains report.createdAt).getOrElse(true) &&
      filter.`type`.map(_ contains report.`type`).getOrElse(true)

  protected implicit def submissionPredicate(filter: Submission.Filter): Submission[T] => Boolean =
    record =>
      filter.period.map(_ contains record.submittedAt).getOrElse(true) &&
      filter.transferTAN.map(_ contains record.metadata.transferTAN).getOrElse(true)

  
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

  def ?(
    id: Id[TransferTAN]
  )(
    implicit env: Env
  ): F[Option[Submission.Report]]


  def ?(filter: Submission.Filter)(
    implicit env: Env
  ): F[Seq[Submission[T]]]


  def history(id: Id[Patient])(
    implicit env: Env
  ): F[Option[History[Submission[T]]]]


  def delete(id: Id[Patient])(
    implicit env: Env
  ): F[Either[String,Unit]]

}
