package de.dnpm.dip.service.validation


import scala.util.Either
import de.dnpm.dip.model.{
  Id,
  Patient
}
import play.api.libs.json.{
  Json,
  Writes
}

object ValidationService
{

  sealed abstract class Command[+T]
  final case class Validate[T](data: T) extends Command[T]
  final case class Delete(patient: Id[Patient]) extends Command[Nothing]

  sealed abstract class Outcome[+T]
  final case class DataValid[T](data: T) extends Outcome[T]
  final case class DataAcceptableWithIssues[T](data: T, report: ValidationReport) extends Outcome[T]
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

  final case class StatusInfo
  (
    total: Int
  )

  object StatusInfo
  {
    implicit val format: Writes[StatusInfo] =
      Json.writes[StatusInfo]
  }

}


trait ValidationService[
  F[_],
  Env,
  PatientRecord
]{
  self =>

  import ValidationService._

  // For use with mere validation,
  // i.e. without the 'side-effect' of actually processing/importing the data set
  def validate(
    patRec: PatientRecord
  )(
    implicit env: Env
  ): F[Either[Error,Outcome[PatientRecord]]]


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
  ): F[Option[PatientRecord]]


  def statusInfo(
    implicit env: Env
  ): F[StatusInfo]

}
