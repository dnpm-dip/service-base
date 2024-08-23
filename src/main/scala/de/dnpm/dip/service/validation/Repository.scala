package de.dnpm.dip.service.validation


import scala.util.Either
import de.dnpm.dip.model.{
  Id,
  Patient
}
//import de.dnpm.dip.service.mvh.SubmissionReport


trait Repository[F[_],Env,PatientRecord]
{

/*
  import scala.language.implicitConversions

  protected implicit def toPredicate(
    filter: SubmissionReport.Filter
  ): SubmissionReport => Boolean =
    report =>
      filter.creationPeriod
        .map(_ contains report.createdAt)
        .getOrElse(true)
*/

  def save(
    data: PatientRecord,
    report: ValidationReport
  )(
    implicit env: Env
  ): F[Either[String,Unit]]


  def ?(
    filter: ValidationService.Filter
  )(
    implicit env: Env
  ): F[Iterable[(PatientRecord,ValidationReport)]]


  def ?(
    id: Id[Patient]
  )(
    implicit env: Env
  ): F[Option[(PatientRecord,ValidationReport)]]

/*
  def save(
    patient: Id[Patient],
    sub: SubmissionReport
  )(
    implicit env: Env
  ): F[Either[String,Unit]]


  def ?(
    filter: SubmissionReport.Filter
  )(
    implicit env: Env
  ): F[Iterable[SubmissionReport]]
*/

  def delete(
    id: Id[Patient]
  )(
    implicit env: Env
  ): F[Either[String,Unit]]


}
