package de.dnpm.dip.service.validation


import de.dnpm.dip.model.{
  Id,
  Patient
}
import de.dnpm.dip.service.DataUpload


trait Repository[F[_],Env,PatientRecord]
{

  def save(
    data: DataUpload[PatientRecord],
    report: ValidationReport
  )(
    implicit env: Env
  ): F[Either[String,Unit]]


  def ?(
    filter: ValidationService.Filter
  )(
    implicit env: Env
  ): F[Iterable[(DataUpload[PatientRecord],ValidationReport)]]


  def ?(
    id: Id[Patient]
  )(
    implicit env: Env
  ): F[Option[(DataUpload[PatientRecord],ValidationReport)]]


  def delete(
    id: Id[Patient]
  )(
    implicit env: Env
  ): F[Either[String,Unit]]


}
