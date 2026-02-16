package de.dnpm.dip.service


import java.time.LocalDateTime
import scala.util.{
  Left,
  Right
}
import cats.Monad
import cats.syntax.either._
import cats.syntax.applicative._
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.traverse._
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
  BroadConsent,
  MVHService
}
import de.dnpm.dip.service.mvh.Submission.Type.{
  Test,
  Addition,
  Correction,
  FollowUp
}



object UsageScope extends Enumeration
{
  type UsageScope = Value

//  val Validation = Value
  val MVGenomSeq = Value("mvgenomseq")
  val Research   = Value("query")
}


object Orchestrator
{
  sealed trait Command[+T]
  final case class Process[T <: PatientRecord]
  (
    upload: DataUpload[T],
//    scopes: Option[Set[UsageScope.Value]] = None
  )
  extends Command[T]

  final case class Delete
  (
    id: Id[Patient],
    scopes: Option[Set[UsageScope.Value]] = None
  )
  extends Command[Nothing]

  sealed trait Outcome
  final case object Saved extends Outcome
  final case class SavedWithIssues(report: ValidationReport) extends Outcome
  final case class Deleted(id: Id[Patient]) extends Outcome


  type Error = Either[ValidationService.Error,Either[MVHService.Error,QueryService.DataError]]

  object Error
  {
    def apply(err: QueryService.DataError): Error =
      err.asRight[MVHService.Error]
        .asRight[ValidationService.Error]

    def apply(err: MVHService.Error): Error =
      err.asLeft[QueryService.DataError]
        .asRight[ValidationService.Error]

    def apply(err: ValidationService.Error): Error =
      err.asLeft[Either[MVHService.Error,QueryService.DataError]]

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
  import UsageScope._
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
  ): F[Either[List[Error],Orchestrator.Outcome]] =
    cmd match {

      case Process(rawData) =>

        for { 

          dataUpload <- rawData.copy(
            // Complete the PatientRecord (resolve display value of Codings etc)
            record = rawData.record.complete,
          )
          .pure // Load pre-processed data into Monad

          validationResult <- validationService ! Validate(dataUpload)

          finalResult <- validationResult match {
            
            // Validation (partially) passed
            case Right(validationOutcome) =>

              // Build a List of transactions/operations constituting the "saga" to be orchestrated:
              val transactions = dataUpload.metadata match {

                // MV submission
                case Some(metadata) =>
                  (mvhService ! MVHService.Process(dataUpload.record,metadata)) :: 
                  metadata.researchConsents
                    .map(BroadConsent.permitsResearchUse)
                    .collect {
                      // If ResearchConsent is given, save the data in the query module, except for submission type 'test'
                      case true if metadata.`type` != Test => queryService ! QueryService.Save(dataUpload.record.deidentified)
                    }
                    .orElse(
                      metadata.`type` match {
                        // For non-initial submissions, delete data from the query module in case it has been saved on initial submission
                        case Addition | Correction | FollowUp => Some(queryService ! QueryService.Delete(dataUpload.record.id))

                        case _ => None  // Nothing to do
                      }
                    )                                  
                    .toList
                  
                // No MV metadata ("DNPM-only"): Only save in query module
                case None => List(queryService ! QueryService.Save(dataUpload.record.deidentified))
              }

              for {

                // Execute the "saga" 
                results <- transactions.sequence

                errors = results.collect {
                  case Left(err: MVHService.Error)       => Error(err)
                  case Left(err: QueryService.DataError) => Error(err)
                }

                outcome = errors.isEmpty match {
                  case true => 
                    validationOutcome match {
                      case DataValid(data)                       => Saved.asRight
                      case DataAcceptableWithIssues(data,report) => SavedWithIssues(report).asRight
                    
                      // Can't occur but required for exhaustive pattern match
                      case ValidationService.Deleted(_) => List(Error(ValidationService.GenericError("Unexpected validation outcome"))).asLeft
                    }

                  case false => errors.asLeft

                }
              
              } yield outcome

            // Validation failed
            case Left(err) => List(Error(err)).asLeft.pure 

          }

        } yield finalResult


      case Orchestrator.Delete(id,optScopes) =>

        val scopes =
          optScopes match { 
            case Some(scopes) if scopes.nonEmpty => values
            case _                               => UsageScope.values.toSet
          }

        for {
          results <- scopes.toList.traverse {
            case MVGenomSeq => mvhService ! MVHService.Delete(id)
            case Research   => queryService ! QueryService.Delete(id)
          }

          validation <- validationService ! ValidationService.Delete(id)

          errors =
            (validation :: results).collect {
              case Left(err: ValidationService.Error) => Error(err)
              case Left(err: MVHService.Error)        => Error(err)
              case Left(err: QueryService.DataError)  => Error(err)
            }

          outcome = errors.isEmpty match {
            case true => Deleted(id).asRight
            case false => errors.asLeft
          }
              
        } yield outcome

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
