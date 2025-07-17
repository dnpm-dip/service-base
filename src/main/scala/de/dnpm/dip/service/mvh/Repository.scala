package de.dnpm.dip.service.mvh


import de.dnpm.dip.model.{
  Id,
  Patient,
  PatientRecord,
}


trait Repository[F[_],Env,T <: PatientRecord]
{

  protected implicit def toPredicate(
    filter: Submission.Report.Filter
  ): Submission.Report => Boolean =
    report =>
      filter.period.map(_ contains report.createdAt).getOrElse(true) &&
      filter.status.map(_ contains report.status).getOrElse(true)


  protected implicit def toPredicate(
    filter: Submission.Filter
  ): Submission[T] => Boolean =
    record =>
      filter.period
        .map(_ contains record.submittedAt)
        .getOrElse(true)

  
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


  def delete(id: Id[Patient])(
    implicit env: Env
  ): F[Either[String,Unit]]

}
