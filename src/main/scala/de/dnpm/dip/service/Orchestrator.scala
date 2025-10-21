package de.dnpm.dip.service


import java.time.LocalDateTime
import scala.util.{
  Left,
  Right
}
import cats.Monad
import cats.syntax.either._
import cats.syntax.apply._
import cats.syntax.applicative._
import cats.syntax.functor._
import cats.syntax.flatMap._
import de.dnpm.dip.util.Completer
import de.dnpm.dip.model.{
  Id,
  Patient,
  PatientRecord,
  Site
}
import de.dnpm.dip.service.validation.{
  ValidationService,
  ValidationReport
}
import de.dnpm.dip.service.query.QueryService
import de.dnpm.dip.service.mvh.{
  MVHService,
  ResearchConsent
}



object Orchestrator
{
  sealed trait Command[+T]
  final case class Process[T <: PatientRecord](upload: DataUpload[T]) extends Command[T]
  final case class Delete(id: Id[Patient]) extends Command[Nothing]

  sealed trait Outcome
  final case object Saved extends Outcome
  final case class SavedWithIssues(report: ValidationReport) extends Outcome
  final case class Deleted(id: Id[Patient]) extends Outcome


  type Error = Either[ValidationService.Error,Either[MVHService.Error,QueryService.DataError]]

  object Error
  {
    def apply[T](err: QueryService.DataError): Either[Error,Outcome] =
      err.asRight[MVHService.Error]
        .asRight[ValidationService.Error]
        .asLeft

    def apply[T](err: MVHService.Error): Either[Error,Outcome] =
      err.asLeft[QueryService.DataError]
        .asRight[ValidationService.Error]
        .asLeft

    def apply[T](err: ValidationService.Error): Either[Error,Outcome] =
      err.asLeft[Either[MVHService.Error,QueryService.DataError]]
        .asLeft
  }

}


final class Orchestrator[F[+_],T <: PatientRecord: Completer]
(
  validationService: ValidationService[F,Monad[F],T],
  mvhService: MVHService[F,Monad[F],T],
  queryService: QueryService.DataOps[F,Monad[F],T]
)
{

  import Orchestrator._
  import ValidationService.{
    Validate,
    DataValid,
    DataAcceptableWithIssues
  }
  import Completer.syntax._


  def !(
    cmd: Orchestrator.Command[T]
  )(
    implicit env: Monad[F]
  ): F[Either[Error,Orchestrator.Outcome]] =
    cmd match {

      case Process(rawData) =>
        for { 

          dataUpload <- rawData.copy(record = rawData.record.complete).pure  // Load completed record into Monad

          validationResult <- (validationService ! Validate(dataUpload))

          finalResult <- validationResult match {
            
            // Validation (partially) passed
            case Right(outcome) =>
              for {
                saveResult <- dataUpload.metadata match {
                  case Some(metadata) =>
                    for {
                      mvhResult <- mvhService ! MVHService.Process(dataUpload.record,metadata)

                      dnpmPermitted =
                        metadata.researchConsents.exists(ResearchConsent.isGiven)
                
                      result <- mvhResult match {
                        case Right(_) =>
                          if (dnpmPermitted) queryService ! QueryService.Save(dataUpload.record)
                          else mvhResult.pure
                
                        case err => err.pure
                      }

                    } yield result

                  case None => queryService ! QueryService.Save(dataUpload.record)
                }
                  
              } yield saveResult match {

                case Right(MVHService.Saved | QueryService.Saved(_)) => 

                  outcome match {
                    case DataValid(data)                       => Saved.asRight
                    case DataAcceptableWithIssues(data,report) => SavedWithIssues(report).asRight

                    // Can't occur but required for exhaustive pattern match
                    case ValidationService.Deleted(_) => Error[T](ValidationService.GenericError("Unexpected validation outcome"))
                  }
            
                case Left(err: MVHService.Error) => Error[T](err)

                case Left(err: QueryService.DataError) => Error[T](err)
            
                // These cases can't occur but are required for exhaustive pattern match:
                case Right(_) => Saved.asRight   // In this case saving has worked
                case Left(_)  => Error[T](QueryService.GenericError("Unexpected data saving outcome"))

              }

            // Validation failed
            case Left(err) => Error[T](err).pure 

          }

        } yield finalResult


      case Orchestrator.Delete(id) =>
        (
          validationService ! ValidationService.Delete(id),
          mvhService ! MVHService.Delete(id),
          queryService ! QueryService.Delete(id)
        )
        .mapN(
          (out,_,_) =>
            out match {
              case Right(_)  => Deleted(id).asRight
              case Left(err) => Error[T](err)
            }
        )
    }


  def statusInfo(
    implicit env: Monad[F]
  ): F[StatusInfo] =
    for {
      validation <- validationService.statusInfo
      mvh <- mvhService.statusInfo
      query <- queryService.statusInfo
    } yield StatusInfo(
      Site.local,
      LocalDateTime.now,
      validation,
      mvh,
      query
    )

}
