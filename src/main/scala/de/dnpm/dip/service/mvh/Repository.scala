package de.dnpm.dip.service.mvh


import de.dnpm.dip.model.{
  Id,
  Patient,
  PatientRecord,
}


trait Repository[F[_],Env,T <: PatientRecord]
{

  import scala.language.implicitConversions

  protected implicit def toPredicate(
    filter: SubmissionReport.Filter
  ): SubmissionReport => Boolean =
    report =>
      filter.creationPeriod
        .map(_ contains report.createdAt)
        .getOrElse(true)

  def save(
    record: MVHPatientRecord[T],
    report: SubmissionReport
  )(
    implicit env: Env
  ): F[Either[String,Unit]]

  def ?(filter: SubmissionReport.Filter)(
    implicit env: Env
  ): F[Iterable[SubmissionReport]]

  def delete(id: Id[Patient])(
    implicit env: Env
  ): F[Either[String,Unit]]

}

