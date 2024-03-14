package de.dnpm.dip.service.validation


import scala.util.Either
import de.dnpm.dip.model.{
  Id,
  Patient
}


trait Repository[F[_],Env,PatientRecord]
{

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
  

  def delete(
    id: Id[Patient]
  )(
    implicit env: Env
  ): F[Either[String,Unit]]


}
