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
  BroadConsent
}


object Orchestrator
{
  sealed trait Command[+T]
  final case class Process[T <: PatientRecord]
  (
    upload: DataUpload[T],
//    deidentifyBroadConsent: Boolean = false
  )
  extends Command[T]

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
)(
  implicit patientSetter: (T,Patient) => T
)
{

  import Orchestrator._
  import ValidationService.{
    Validate,
    DataValid,
    DataAcceptableWithIssues
  }
  import Completer.syntax._
  import Deidentifier.syntax._


  // PatientRecord Deidentifier:
  // - Remove MV-specific element Patient.address in PatientRecords transferred into the Query module
  private implicit lazy val recordDeidentifier: Deidentifier.Of[T] =
    (record: T) =>
       patientSetter(record,record.patient.copy(address = None))


  def !(
    cmd: Orchestrator.Command[T]
  )(
    implicit env: Monad[F]
  ): F[Either[Error,Orchestrator.Outcome]] =
    cmd match {

      case Process(rawData) =>

        for { 

          dataUpload <- rawData.copy(

            // Complete the PatientRecord (resolve display value of Codings etc)
            record = rawData.record.complete,
/*
            // Deidentify BroadConsent, if specified by the client
            metadata =
              if (deidentifyBroadConsent)
                rawData.metadata.map(
                  m => m.copy(
                    researchConsents = m.researchConsents.map(_.map(_.deidentifiedWith(rawData.record.patient.id)))
                  )
                )
              else rawData.metadata
*/              
          )
          .pure  // Load pre-processed data into Monad

          validationResult <- (validationService ! Validate(dataUpload))

          finalResult <- validationResult match {
            
            // Validation (partially) passed
            case Right(outcome) =>

              for {
                saveResult <- dataUpload.metadata match {
                  case Some(metadata) =>
                    for {
                      mvhResult <- mvhService ! MVHService.Process(dataUpload.record,metadata)

                      dnpmPermitted = metadata.researchConsents.exists(BroadConsent.permitsResearchUse)
                
                      result <- mvhResult match {
                        case Right(_) =>
                          if (dnpmPermitted) queryService ! QueryService.Save(dataUpload.record.deidentified)
                          else mvhResult.pure

                        case err => err.pure
                      }

                    } yield result

                  case None => queryService ! QueryService.Save(dataUpload.record.deidentified)
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
