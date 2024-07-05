package de.dnpm.dip.service.validation


import java.time.Instant
import scala.util.{
  Either,
  Left,
  Right
}
import scala.concurrent.{
  ExecutionContext,
  Future
}
import cats.data.{
  Validated,
  ValidatedNel
}
import cats.{
  Applicative,
  Monad
}
import de.dnpm.dip.util.Logging
import de.dnpm.dip.model.{
  Id,
  Patient,
}
import de.dnpm.dip.service.Data.{
  Error,
  FatalIssuesDetected,
  UnacceptableIssuesDetected,
  GenericError
}
import Issue.Severity
import de.ekut.tbi.validation.Validator



private final case class SeverityMatcher(
  max: Severity.Value
){
  def unapply(report: ValidationReport): Boolean =
    report.issues.forall(
      issue => issue.severity < max
    )
}

private object FatalIssues
{
  def unapply(report: ValidationReport): Boolean =
    report.issues.exists(_.severity == Severity.Fatal)
}

class BaseValidationService[
  F[+_],
  PatientRecord <: { def patient: Patient }
](
  private val validator: Validator[Issue,PatientRecord],
  private val repo: Repository[F,Monad[F],PatientRecord],
  private val maxSeverity: Severity.Value = Severity.Error
)
extends ValidationService[F,Monad[F],PatientRecord]
with Logging
{


  import ValidationService._

  import cats.syntax.apply._
  import cats.syntax.either._
  import cats.syntax.functor._
  import cats.syntax.applicative._
  import cats.syntax.flatMap._
  import scala.language.reflectiveCalls


  private val Acceptable =
    SeverityMatcher(maxSeverity)


  override def validate(
    data: PatientRecord
  )(
    implicit env: Monad[F]
  ): F[Either[Error,Outcome[PatientRecord]]] = {

    log.info("Performing simple PatientRecord validation (non-importing))")

    for {
      validationResult <- validator(data).pure

      result =
        validationResult match { 
          case Validated.Valid(patientRecord) =>
            DataValid(data).asRight[Error]
          
          case Validated.Invalid(issues) =>
            val report =
              ValidationReport(
                data.patient.id,
                issues,
                Instant.now
              )          
          
            report match {
              case Acceptable()  => DataAcceptableWithIssues(data,report).asRight[Error]
              case FatalIssues() => FatalIssuesDetected(report).asLeft[Outcome[PatientRecord]]
              case _             => UnacceptableIssuesDetected(report).asLeft[Outcome[PatientRecord]]
            }
          
        }

    } yield result
  }

  override def !(
    cmd: Command[PatientRecord]
  )(
    implicit env: Monad[F] 
  ): F[Either[Error,Outcome[PatientRecord]]] =
    cmd match {

      case Validate(data) =>
        log.info(s"Validating PatientRecord ${data.patient.id} before import")
        for {
          outcome <- validate(data)

          result <- outcome match { 
            case Right(DataValid(_)) =>
              log.debug(s"Data valid: deleting previously saved validation reports")
              // In case this were a consecutive export which now turns out valid,
              // delete the patient's previously saved validationReport and record
              repo.delete(data.patient.id)
              outcome.pure
            
            case Right(DataAcceptableWithIssues(_,report)) =>
              log.debug(s"Data acceptable but with with issues: Saving data set and validation report")
              repo.save(data,report)
                .map {
                  case Right(_)  => outcome
                  case Left(err) => GenericError(err).asLeft[Outcome[PatientRecord]]
                }

            case Left(UnacceptableIssuesDetected(report)) =>
              log.debug(s"Unacceptable issues: Saving data set and validation report")
              repo.save(data,report)
                .map {
                  case Right(_)  => outcome
                  case Left(err) => GenericError(err).asLeft[Outcome[PatientRecord]]
                }

            case Left(_) => outcome.pure

            // Won't occur but required for exhaustive pattern match
            case Right(Deleted(_)) =>
              GenericError("Unexpected validation outcome").asLeft[Outcome[PatientRecord]].pure
          }

        } yield result


      case Delete(id) =>
        log.info(s"Deleting all data of Patient $id")
        repo.delete(id).map {
          case Right(_)  => Deleted(id).asRight[Error]
          case Left(err) => GenericError(err).asLeft[Outcome[PatientRecord]]
        }

    }


  override def ?(
    filter: Filter
  )(
    implicit env: Monad[F]
  ): F[Seq[ValidationInfo]] =
    (repo ? filter).map(
      _.map {
        case (_,validationReport) =>
          ValidationInfo(
            validationReport.patient,
            validationReport.issues
              .toList
              .groupBy(_.severity)
              .map {
                case (severity,issues) => severity.toString -> issues.size
              }
          )
      }
      .toSeq
    )


  override def dataQualityReport(
    patId: Id[Patient]
  )(
    implicit env: Monad[F]
  ): F[Option[ValidationReport]] = 
    (repo ? patId).map(_.map(_._2))


  override def patientRecord(
    patId: Id[Patient]
  )(
    implicit env: Monad[F]
  ): F[Option[PatientRecord]] =
    (repo ? patId).map(_.map(_._1))


}
