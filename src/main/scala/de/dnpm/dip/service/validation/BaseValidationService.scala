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
import Issue.Severity
import de.ekut.tbi.validation.Validator



private final case class SeverityMatcher(
  threshold: Severity.Value
){
  def unapply(report: ValidationReport): Boolean =
    report.issues.forall(
      issue => Ordering[Severity.Value].lteq(issue.severity,threshold)
    )
}


object FatalIssues
{
  def unapply(report: ValidationReport): Boolean =
    report.issues.exists(_.severity == Issue.Severity.Fatal)
}



class BaseValidationService[
  F[+_],
  PatientRecord <: { def patient: Patient }
](
  private val validator: Validator[Issue,PatientRecord],
  private val severityThreshold: Severity.Value,
  private val repo: Repository[F,Monad[F],PatientRecord]
)
extends ValidationService[
  F,Monad[F],PatientRecord
]{

  import ValidationService._

  import cats.syntax.apply._
  import cats.syntax.either._
  import cats.syntax.functor._
  import cats.syntax.applicative._
  import cats.syntax.flatMap._
  import scala.language.reflectiveCalls


  private val Acceptable =
    SeverityMatcher(severityThreshold)


  override def validate(
    data: PatientRecord
  )(
    implicit env: Monad[F]
  ): F[Outcome[PatientRecord]] =
    validator(data) match { 

      case Validated.Valid(patientRecord) =>
        DataValid(data)
          .pure[F]

      case Validated.Invalid(issues) =>

        val report =
          ValidationReport(
            data.patient.id,
            issues,
            Instant.now
          )          

        DataInvalid(data,report)
          .pure[F]

    }



  override def !(
    cmd: Command[PatientRecord]
  )(
    implicit env: Monad[F] 
  ): F[Either[Error,Outcome[PatientRecord]]] = {


    cmd match {

      // TODO: Logging

      case Validate(data) =>
        validate(data)
          .flatMap { 
            case v: DataValid[PatientRecord] =>
              v.asRight[Error]
               .pure[F]
            
            case inv @ DataInvalid(data,report) =>
              report match {
            
                case FatalIssues() =>
                  DataFatallyInvalid(report)
                    .asRight[Error]
                    .pure[F]
            
                case _ =>    
                  repo.save(data,report)
                    .map {
                      case Right(_) =>
                        inv.asRight[Error]
                    
                      case Left(err1) =>
                        UnspecificError(err1)
                          .asLeft[Outcome[PatientRecord]]
                  }
            
              }

            case _ =>
              UnspecificError("Unexpected validation outcome")
                .asLeft[Outcome[PatientRecord]]
                .pure[F]

          }
/*
        validator(data) match { 

          case Validated.Valid(patientRecord) =>
            DataValid(data)
              .asRight[Error]
              .pure[F]


          case Validated.Invalid(issues) =>

            val report =
              ValidationReport(
                data.patient.id,
                issues,
                Instant.now
              )          

            report match {

              case FatalIssues() =>
                DataFatallyInvalid(report)
                  .asRight[Error]
                  .pure[F]

              case _ =>    
                repo.save(data,report)
                  .map {
                    case Right(_) =>
                      DataInvalid(data,report)
                        .asRight[Error]
                  
                    case Left(err1) =>
                      UnspecificError(err1)
                        .asLeft[Outcome[PatientRecord]]
                }

            }
        }
*/


      case Delete(id) =>
        repo.delete(id).map {
          case Right(_)  => Deleted(id).asRight[Error]

          case Left(err) => UnspecificError(err).asLeft[Outcome[PatientRecord]]
        }

    }

  }


  override def ?(
    filter: Filter
  )(
    implicit env: Monad[F]
  ): F[Iterable[DataValidationInfo]] =
    (repo ? filter).map(
      _.map {
        case (_,validationReport) =>
          DataValidationInfo(
            validationReport.patient,
            validationReport.issues
              .toList
              .groupBy(_.severity)
              .map {
                case (severity,issues) => severity -> issues.size
              }
          )
      }
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
