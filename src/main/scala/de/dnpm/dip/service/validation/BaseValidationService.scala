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
import cats.Applicative
import de.dnpm.dip.util.Logging
import de.dnpm.dip.model.{
  Id,
  Patient,
}
import ValidationReport.Issue.Severity


trait DataValidator[T] extends (T => ValidatedNel[ValidationReport.Issue,T])


final case class SeverityMatcher(
  threshold: ValidationReport.Issue.Severity.Value
){
  def unapply(report: ValidationReport): Boolean =
    report.issues.forall(
      issue => Ordering[Severity.Value].lteq(issue.severity,threshold)
    )
}


object FatalIssues
{
  def unapply(report: ValidationReport): Boolean =
    report.issues.exists(_.severity == ValidationReport.Issue.Severity.Fatal)
}



class BaseValidationService[
  F[+_],
  PatientRecord <: { val patient: Patient }
](
  private val validator: DataValidator[PatientRecord],
  private val severityThreshold: Severity.Value,
  private val repo: Repository[F,Applicative[F],PatientRecord]
)
extends ValidationService[
  F,Applicative[F],PatientRecord
]{

  import ValidationService._

  import cats.syntax.apply._
  import cats.syntax.either._
  import cats.syntax.functor._
  import cats.syntax.applicative._


  private val Acceptable = SeverityMatcher(severityThreshold)


  override def !(
    cmd: Command[PatientRecord]
  )(
    implicit env: Applicative[F] 
  ): F[Either[Error,Outcome[PatientRecord]]] = {

    import scala.language.reflectiveCalls

    cmd match {

      // TODO: Logging

      case Validate(data) =>

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


      case Delete(id) =>
        repo.delete(id).map {
          case Left(err) =>
            UnspecificError(err).asLeft[Outcome[PatientRecord]]

          case Right(_)  =>
            Deleted(id).asRight[Error]
        }

    }

  }


  override def ?(
    filter: Filter
  )(
    implicit env: Applicative[F]
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
    implicit env: Applicative[F]
  ): F[Option[ValidationReport]] = 
    (repo ? patId).map(_.map(_._2))


  override def patientRecord(
    patId: Id[Patient]
  )(
    implicit env: Applicative[F]
  ): F[Option[PatientRecord]] =
    (repo ? patId).map(_.map(_._1))


}
