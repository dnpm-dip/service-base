package de.dnpm.dip.service.validation


import scala.util.Either
import de.dnpm.dip.model.{
  Id,
  Patient
}
import de.dnpm.dip.service.DataUpload


object ValidationService
{

  sealed abstract class Command[+T]
  final case class Validate[T](data: DataUpload[T], persist: Boolean) extends Command[T]
  final case class Delete(patient: Id[Patient]) extends Command[Nothing]

  sealed abstract class Outcome[+T]
  final case class DataValid[T](data: DataUpload[T]) extends Outcome[T]
  final case class DataAcceptableWithIssues[T](data: DataUpload[T], report: ValidationReport) extends Outcome[T]
  final case class Deleted(patient: Id[Patient]) extends Outcome[Nothing]

  sealed trait Error
  final case class FatalIssuesDetected(report: ValidationReport) extends Error
  final case class UnacceptableIssuesDetected(report: ValidationReport) extends Error
  final case class GenericError(msg: String) extends Error


  final case class Filter
  (
    severities: Option[Set[Issue.Severity.Value]] = None
  )

  object Filter
  {
    val empty = Filter(None)
  }

}


trait ValidationService[F[_],Env,PatientRecord]{

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
  ): F[Seq[ValidationInfo]]


  def dataQualityReport(
    patId: Id[Patient]
  )(
    implicit env: Env
  ): F[Option[ValidationReport]]


  def patientRecord(
    patId: Id[Patient]
  )(
    implicit env: Env
  ): F[Option[DataUpload[PatientRecord]]]

}
