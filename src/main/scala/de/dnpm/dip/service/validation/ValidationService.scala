package de.dnpm.dip.service.validation


import scala.util.Either
import cats.data.{
  NonEmptyList
}
import de.dnpm.dip.model.{
  Id,
  Gender,
  Patient,
}


object ValidationService
{

  sealed abstract class Command[+T]
  final case class Validate[T](data: T) extends Command[T]

  final case class Delete(patient: Id[Patient]) extends Command[Nothing]


  sealed abstract class Outcome[+T]

  final case class DataValid[T](data: T) extends Outcome[T]

  final case class DataFatallyInvalid(report: ValidationReport) extends Outcome[Nothing]

  final case class DataInvalid[T](
    data: T,
    report: ValidationReport
  ) extends Outcome[T]
 
  final case class Deleted(patient: Id[Patient]) extends Outcome[Nothing]

  
  sealed abstract class Error

  final case class UnspecificError(msg: String) extends Error


  final case class Filter
  (
    severities: Option[Set[ValidationReport.Issue.Severity.Value]]
  )

  object Filter
  {
    val empty = Filter(None)
  }

}


trait ValidationService[
  F[_],
  Env,
  PatientRecord
]{
  self =>

  import ValidationService._

  def !(
    cmd: Command[PatientRecord]
  )(
    implicit env: Env
  ): F[Either[Error,Outcome[PatientRecord]]]


  def ?(
    filter: Filter
  )(
    implicit env: Env
  ): F[Iterable[DataValidationInfo]]


  def ?(
    implicit env: Env
  ): F[Iterable[DataValidationInfo]] =
    self ? Filter.empty


  def dataQualityReport(
    patId: Id[Patient]
  )(
    implicit env: Env
  ): F[Option[ValidationReport]]


  def patientRecord(
    patId: Id[Patient]
  )(
    implicit env: Env
  ): F[Option[PatientRecord]]

}

