package de.dnpm.dip.service.validation


import scala.util.Either
import cats.data.NonEmptyList
import de.dnpm.dip.model.{
  Id,
  Patient,
}
import de.dnpm.dip.service.Data.Error


object ValidationService
{

  sealed abstract class Command[+T]
  final case class Validate[T](data: T) extends Command[T]
  final case class Delete(patient: Id[Patient]) extends Command[Nothing]


  sealed abstract class Outcome[+T]
  final case class DataValid[T](data: T) extends Outcome[T]
  final case class DataAcceptableWithIssues[T](data: T, report: ValidationReport) extends Outcome[T]
  final case class Deleted(patient: Id[Patient]) extends Outcome[Nothing]


  final case class Filter
  (
    severities: Option[Set[Issue.Severity.Value]]
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

}

