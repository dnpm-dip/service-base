package de.dnpm.dip.service.validation


import java.time.Instant
import scala.util.{
  Either,
  Left,
  Right
}
import cats.data.{
  Validated,
}
import cats.Monad
import de.dnpm.dip.util.Logging
import de.dnpm.dip.model.{
  Id,
  Patient,
  PatientRecord,
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
  T <: PatientRecord,
](
  private val validator: Validator[Issue,T],
  private val repo: Repository[F,Monad[F],T],
  private val maxSeverity: Severity.Value = Severity.Error
)
extends ValidationService[F,Monad[F],T]
with Logging
{

  import cats.syntax.either._
  import cats.syntax.functor._
  import cats.syntax.applicative._
  import cats.syntax.flatMap._
  import ValidationService._


  private val Acceptable =
    SeverityMatcher(maxSeverity)


  override def validate(
    data: T
  )(
    implicit env: Monad[F]
  ): F[Either[Error,Outcome[T]]] = {

    log.info("Performing PatientRecord validation")

    for {
      validationResult <- validator(data).pure

      result =
        validationResult match { 
          case Validated.Valid(_) =>
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
              case FatalIssues() => FatalIssuesDetected(report).asLeft[Outcome[T]]
              case _             => UnacceptableIssuesDetected(report).asLeft[Outcome[T]]
            }
          
        }

    } yield result
  }


  override def !(
    cmd: Command[T]
  )(
    implicit env: Monad[F] 
  ): F[Either[Error,Outcome[T]]] =
    cmd match {

      case Validate(record) =>
        log.info(s"Processing PatientRecord upload ${record.id}")
        for {
          outcome <- validate(record)

          result <- outcome match { 
            case Right(DataValid(_)) =>
              log.debug(s"Data valid: deleting previously saved validation reports")
              // In case this were a consecutive export which now turns out valid,
              // delete the patient's previously saved validationReport and record
              repo.delete(record.patient.id)
              outcome.pure
            
            case Right(DataAcceptableWithIssues(_,report)) =>
              log.debug(s"Data acceptable but with with issues: Saving record set and validation report")
              repo.save(record,report)
                .map {
                  case Right(_)  => outcome
                  case Left(err) => GenericError(err).asLeft[Outcome[T]]
                }

            case Left(UnacceptableIssuesDetected(report)) =>
              log.debug(s"Unacceptable issues: Saving record set and validation report")
              repo.save(record,report)
                .map {
                  case Right(_)  => outcome
                  case Left(err) => GenericError(err).asLeft[Outcome[T]]
                }

            case Left(_) => outcome.pure

            // Won't occur but required for exhaustive pattern match
            case Right(Deleted(_)) =>
              GenericError("Unexpected validation outcome").asLeft[Outcome[T]].pure
          }

        } yield result


      case Delete(id) =>
        log.info(s"Deleting all data of Patient $id")
        repo.delete(id).map {
          case Right(_)  => Deleted(id).asRight[Error]
          case Left(err) => GenericError(err).asLeft[Outcome[T]]
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
              },
            validationReport.createdAt
          )
      }
      .toSeq
      .sorted(Ordering[ValidationInfo].reverse) // Reverse ordering to have "greater" entries (i.e. having more errors) at the beginning
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
  ): F[Option[T]] =
    (repo ? patId).map(_.map(_._1))

  override def statusInfo(
    implicit env: Monad[F]
  ): F[StatusInfo] =
    (repo ? Filter())
      .map(_.size)
      .map(StatusInfo(_))

}
