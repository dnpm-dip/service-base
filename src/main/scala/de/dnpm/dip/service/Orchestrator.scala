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
import de.dnpm.dip.util.{
  Completer,
  Logging
}
import de.dnpm.dip.model.{
  Id,
  Patient,
  PatientRecord,
  Snapshot
}
import de.dnpm.dip.service.validation.{
  ValidationService,
  ValidationReport
}
import de.dnpm.dip.service.query.{
  UseCaseConfig,
  QueryService
}
import de.dnpm.dip.service.mvh.{
  Metadata,
  MVHPatientRecord,
  MVHService
}


object Data
{

  sealed trait Command[+T]
  final case class Save[T](record: T) extends Command[T]
  final case class Delete(id: Id[Patient]) extends Command[Nothing]

  sealed trait Outcome[+T]
  final case class Saved[T](snp: Snapshot[T]) extends Outcome[T]
  final case class SavedWithIssues[T](snp: Snapshot[T], report: ValidationReport) extends Outcome[T]
  final case class Deleted(id: Id[Patient]) extends Outcome[Nothing]

  sealed trait Error
  final case class FatalIssuesDetected(report: ValidationReport) extends Error
  final case class UnacceptableIssuesDetected(report: ValidationReport) extends Error
  final case class GenericError(msg: String) extends Error


  trait Ops[F[_],Env,PatientRecord]
  {
    def !(cmd: Command[PatientRecord])(
      implicit env: Env
    ): F[Either[Error,Outcome[PatientRecord]]]

  }

}


object Orchestrator
{
  sealed trait Command[+T]
  final case class Process[T <: PatientRecord](record: T,meta: Option[Metadata] = None) extends Command[T]
  final case class Delete(id: Id[Patient]) extends Command[Nothing]
}

final class Orchestrator[F[_],T <: PatientRecord: Completer]
(
  validationService: ValidationService[F,Monad[F],T],
  mvhService: MVHService[F,Monad[F],T],
  queryService: Data.Ops[F,Monad[F],T]
)
{

  import Data._
  import Orchestrator.Process
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
  ): F[Either[Error,Data.Outcome[T]]] =
    cmd match {
      case Process(rawRecord,mvhMetadata) =>
        for { 
          record <- rawRecord.complete.pure  // Load completed record into Monad

          validationOutcome <- (validationService ! Validate(record))

          _ =
            mvhMetadata.foreach {
              meta =>
                mvhService ! MVHService.Process(
                  MVHPatientRecord(record,meta),
                  validationOutcome match {
                    case Right(_) => true
                    case Left(_)  => false
                  }
                )
            }

          result <- validationOutcome match {
            case Right(DataValid(_)) =>
              queryService ! Save(record)

            case Right(DataAcceptableWithIssues(_,report)) =>
              (queryService ! Save(record)).map(
                _.flatMap {
                  case Saved(snp) => SavedWithIssues(snp,report).asRight[Error]

                  // Can't occur but required for exhaustive pattern match
                  case _          => GenericError("Unexpected save outcome").asLeft[Outcome[T]]
                }
              )
             
            // Can't occur but required for exhaustive pattern match
            case Right(ValidationService.Deleted(_))  =>
              GenericError("Unexpected validation outcome").asLeft[Outcome[T]].pure

            case Left(err)  =>
              err.asLeft[Outcome[T]].pure
            }

        } yield result


      case Orchestrator.Delete(id) =>
        (
          validationService ! ValidationService.Delete(id),
          queryService ! Delete(id)
        )
        .mapN(
          (out,_) =>
            out.bimap(
              err => GenericError(err.toString),
              _   => Deleted(id)
            )
        )
    }

}

/*
final class Orchestrator[F[_],PatientRecord: Completer]
(
  validationService: ValidationService[F,Monad[F],PatientRecord],
  queryService: Data.Ops[F,Monad[F],PatientRecord],
)
extends Data.Ops[F,Monad[F],PatientRecord]
{

  import Data._ 
  import ValidationService.{
    Validate,
    DataValid,
    DataAcceptableWithIssues
  }
  import Completer.syntax._

  
  override def !(
    cmd: Data.Command[PatientRecord]
  )(
    implicit env: Monad[F]
  ): F[Either[Error,Data.Outcome[PatientRecord]]] =
    cmd match {
      case Save(rawRecord,meta) =>
        for { 
          record <- rawRecord.complete.pure  // Load completed record into Monad

          validationOutcome <- (validationService ! Validate(record))

          result <- validationOutcome match {
            case Right(DataValid(_)) =>
              queryService ! Save(record)

            case Right(DataAcceptableWithIssues(_,report)) =>
              (queryService ! Save(record)).map(
                _.flatMap {
                  case Saved(snp) => SavedWithIssues(snp,report).asRight[Error]

                  // Can't occur but required for exhaustive pattern match
                  case _          => GenericError("Unexpected save outcome").asLeft[Outcome[PatientRecord]]
                }
              )
             
            // Can't occur but required for exhaustive pattern match
            case Right(ValidationService.Deleted(_))  =>
              GenericError("Unexpected validation outcome").asLeft[Outcome[PatientRecord]].pure

            case Left(err)  =>
              err.asLeft[Outcome[PatientRecord]].pure
            }

        } yield result


      case Delete(id) =>
        (
          validationService ! ValidationService.Delete(id),
          queryService ! Delete(id)
        )
        .mapN(
          (out,_) =>
            out.bimap(
              err => GenericError(err.toString),
              _   => Deleted(id)
            )
        )
    }

}
*/
