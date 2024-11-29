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
  Snapshot
}
import de.dnpm.dip.service.validation.{
  ValidationService,
  ValidationReport
}
import de.dnpm.dip.service.query.QueryService
import de.dnpm.dip.service.mvh.{
  Consent,
  Metadata,
  MVHPatientRecord,
  MVHService,
  SubmissionType,
  TransferTAN
}


final case class DataUpload[T]
(
  record: T,
  meta: Option[Metadata]
)


object DataUpload
{

  import play.api.libs.json.{
    JsPath,
    JsObject,
    Reads
  }
  import play.api.libs.functional.syntax._

  implicit def reads[T: Reads]: Reads[DataUpload[T]] =
    (
      JsPath.read[T] and
      (JsPath \ "metadata").readNullable[Metadata]
    )(
      DataUpload(_,_)
    )


  object Schemas {

    import json.{
      Json,
      Schema
    }

    implicit val submissionTypeSchema: Schema[SubmissionType.Value] =
      Json.schema[SubmissionType.Value]
        .toDefinition("MVH_SubmissionType")

    implicit val ttanIdSchema: Schema[Id[TransferTAN]] =
      Schema.`string`.asInstanceOf[Schema[Id[TransferTAN]]]
        .toDefinition("TransferTAN")

    implicit val consentSchema: Schema[Consent] =
      Schema.`object`.Free[JsObject]()
        .asInstanceOf[Schema[Consent]]
        .toDefinition("MVH_Consent")
    
    implicit val metadataSchema: Schema[Metadata] =
      Json.schema[Metadata]
        .toDefinition("MVH_Metadata")
    
//    implicit def schema[T <: PatientRecord](  
    implicit def schema[T <: Product](
      implicit sch: Schema[T]
    ): Schema[DataUpload[T]] =
      (
        sch match {
          case obj: Schema.`object`[T] =>
            obj.withField(
              "metadata",
              Schema[Metadata],
              false
            )
    
          case _ => ??? // Cannot occur due to type bound T <: Product, but required for exhaustive pattern match
        }
      )
      .asInstanceOf[Schema[DataUpload[T]]]
  }

}


object Orchestrator
{
  sealed trait Command[+T]
  final case class Process[T <: PatientRecord](upload: DataUpload[T]) extends Command[T]
  final case class Delete(id: Id[Patient]) extends Command[Nothing]

  sealed trait Outcome[+T]
  final case class Saved[T](snp: Snapshot[T]) extends Outcome[T]
  final case class SavedWithIssues[T](snp: Snapshot[T], report: ValidationReport) extends Outcome[T]
  final case class Deleted(id: Id[Patient]) extends Outcome[Nothing]


  type Error = Either[ValidationService.Error,QueryService.DataError]

  object Error
  {
    def apply[T](err: QueryService.DataError): Either[Error,Outcome[T]] =
      err.asRight[ValidationService.Error].asLeft[Outcome[T]]

    def apply[T](err: ValidationService.Error): Either[Error,Outcome[T]] =
      err.asLeft[QueryService.DataError].asLeft[Outcome[T]]
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

  
  def !(
    cmd: Orchestrator.Command[T]
  )(
    implicit env: Monad[F]
  ): F[Either[Error,Orchestrator.Outcome[T]]] =
    cmd match {
      case Process(DataUpload(rawRecord,mvhMetadata)) =>
        for { 
          record <- rawRecord.pure.map(_.complete)  // Load record into Monad

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
              (queryService ! QueryService.Save(record)).map {
                case Right(QueryService.Saved(snp)) => Saved(snp).asRight[Error]

                // Can't occur but required for exhaustive pattern match
                case Right(_)  => Error[T](QueryService.GenericError("Unexpected save outcome"))
                 
                case Left(err) => Error[T](err)
              }

            case Right(DataAcceptableWithIssues(_,report)) =>
              (queryService ! QueryService.Save(record)).map {
                case Right(QueryService.Saved(snp)) => SavedWithIssues(snp,report).asRight[Error]

                // Can't occur but required for exhaustive pattern match
                case Right(_)  => Error[T](QueryService.GenericError("Unexpected save outcome"))
                 
                case Left(err) => Error[T](err)
              }

            case Left(err)  =>
              Error[T](err).pure

            // Can't occur but required for exhaustive pattern match
            case Right(_: ValidationService.Deleted)  =>
              Error[T](ValidationService.GenericError("Unexpected validation outcome")).pure

          }

        } yield result


      case Orchestrator.Delete(id) =>
        (
          validationService ! ValidationService.Delete(id),
          queryService ! QueryService.Delete(id)
        )
        .mapN(
          (out,_) =>
            out match {
              case Right(_)  => Deleted(id).asRight[Error]
              case Left(err) => Error[T](err)
            }
        )
    }

}
