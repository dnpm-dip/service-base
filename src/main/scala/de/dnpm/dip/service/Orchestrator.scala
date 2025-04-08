package de.dnpm.dip.service


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
//  Snapshot
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

  sealed trait Outcome[+T]
//  final case class Saved[T](snp: Snapshot[T]) extends Outcome[T]
//  final case class SavedWithIssues[T](snp: Snapshot[T], report: ValidationReport) extends Outcome[T]
  final case class Saved[T](data: T) extends Outcome[T]
  final case class SavedWithIssues[T](data: T, report: ValidationReport) extends Outcome[T]
  final case class Deleted(id: Id[Patient]) extends Outcome[Nothing]

/*
  type Error = Either[ValidationService.Error,QueryService.DataError]

  object Error
  {
    def apply[T](err: QueryService.DataError): Either[Error,Outcome[T]] =
      err.asRight[ValidationService.Error]
        .asLeft//[Outcome[T]]

    def apply[T](err: ValidationService.Error): Either[Error,Outcome[T]] =
      err.asLeft[QueryService.DataError]
        .asLeft//[Outcome[T]]
  }
*/

  type Error = Either[ValidationService.Error,Either[MVHService.Error,QueryService.DataError]]

  object Error
  {
    def apply[T](err: QueryService.DataError): Either[Error,Outcome[T]] =
      err.asRight[MVHService.Error]
        .asRight[ValidationService.Error]
        .asLeft

    def apply[T](err: MVHService.Error): Either[Error,Outcome[T]] =
      err.asLeft[QueryService.DataError]
        .asRight[ValidationService.Error]
        .asLeft

    def apply[T](err: ValidationService.Error): Either[Error,Outcome[T]] =
      err.asLeft[Either[MVHService.Error,QueryService.DataError]]
        .asLeft
  }
}


final class Orchestrator[F[_],T <: PatientRecord: Completer]
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
  import ResearchConsent.{ 
    MDAT_STORE_AND_PROCESS,
    MDAT_RESEARCH_USE,
    PATDAT_STORE_AND_USE
  }
  
  def !(
    cmd: Orchestrator.Command[T]
  )(
    implicit env: Monad[F]
  ): F[Either[Error,Orchestrator.Outcome[T]]] =
    cmd match {

      case Process(DataUpload(rawRecord,optMetadata)) =>
        for { 
          record <- rawRecord.pure.map(_.complete)  // Load record into Monad

          validationResult <- (validationService ! Validate(record))

          result <- validationResult match {

            // Validation (partially) passed
            case Right(outcome) =>

              for {
                saveResult <- optMetadata match {

                  case Some(metadata) =>

                    val hasQueryBroadConsent =
                      metadata.researchConsents
                        .exists(
                          _.forall(
                            consent => consent.permits(PATDAT_STORE_AND_USE) || (consent.permits(MDAT_STORE_AND_PROCESS) && consent.permits(MDAT_RESEARCH_USE)) 
                          )
                        )

                    if (hasQueryBroadConsent)
                      // MVH and DNPM upload
                      for {
                        _ <- mvhService ! MVHService.Process(record,metadata)
                        res <- queryService ! QueryService.Save(record)
                      } yield res

                    else // MVH-only upload
                      mvhService ! MVHService.Process(record,metadata)
                
                  // DNPM-only upload
                  case None =>
                    queryService ! QueryService.Save(record)
                
                }

              } yield saveResult match {

                case MVHService.Saved | QueryService.Saved => 
                  outcome match {
                    case DataValid(data)                       => Saved(data).asRight[Error]
                    case DataAcceptableWithIssues(data,report) => SavedWithIssues(data,report).asRight[Error]

                    // Can't occur but required for exhaustive pattern match
                    case ValidationService.Deleted(_) => Error[T](ValidationService.GenericError("Unexpected validation outcome"))
                  }

                // Can't occur but required for exhaustive pattern match
                case _  => Error[T](QueryService.GenericError("Unexpected save outcome"))
              }

            // Validation failed
            case Left(err) => Error[T](err).pure
          }
              
        } yield result


      case Orchestrator.Delete(id) =>
        (
          validationService ! ValidationService.Delete(id),
          mvhService ! MVHService.Delete(id),
          queryService ! QueryService.Delete(id)
        )
        .mapN(
          (out,_,_) =>
            out match {
              case Right(_)  => Deleted(id).asRight[Error]
              case Left(err) => Error[T](err)
            }
        )
    }

}
