package de.dnpm.dip.service


import java.time.LocalDateTime
import scala.util.{
  Left,
  Right
}
import cats.Monad
import cats.data.{
  EitherNel,
  Ior,
  NonEmptyList
}
import cats.syntax.either._
import cats.syntax.apply._
import cats.syntax.applicative._
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.traverse._
import de.dnpm.dip.util.Completer
import de.dnpm.dip.coding.Coding
import de.dnpm.dip.model.{
  Id,
  Patient,
  PatientRecord,
  Site
}
import de.dnpm.dip.service.controlling.{ 
  Controlling,
  LocalControllingInfo,
  FederatedControllingInfo
}
import de.dnpm.dip.service.validation.{
  ValidationService,
  ValidationReport
}
import de.dnpm.dip.service.query.QueryService
import de.dnpm.dip.service.mvh.{
  BroadConsent,
  MVHService,
  Submission
}
import de.dnpm.dip.service.mvh.Submission.Type.{
  Test,
  Addition,
  Correction,
  FollowUp,
}
import shapeless.{
  :+:, CNil
}
import shapeless.ops.coproduct.Inject


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
    persistValidationOutcome: Option[Boolean] = None
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


  type Error = ValidationService.Error :+: MVHService.Error :+: QueryService.DataError :+: CNil

  object Error
  {
    def apply[T](t: T)(implicit inj: Inject[Error,T]): Error = inj(t)

    object ops
    {
      import cats.Functor
      import cats.syntax.functor._

      implicit class EitherOps[F[_],E,V](val outcome: F[Either[E,V]]) extends AnyVal
      {
        def adaptError(
          implicit
          func: Functor[F],
          inj: Inject[Error,E]
        ): F[EitherNel[Error,V]] =
          outcome.map(_.leftMap(err => NonEmptyList.one(inj(err))))
      }

      implicit class EitherNelOps[F[_],E,V](val outcome: F[EitherNel[E,V]]) extends AnyVal
      {
        def adaptErrors(
          implicit
          func: Functor[F],
          inj: Inject[Error,E]
        ): F[EitherNel[Error,V]] =
          outcome.map(_.leftMap(_.map(inj(_))))
      }
    }
  }

}


final class Orchestrator[F[+_],T <: PatientRecord: Completer]
(
  validationService: ValidationService[F,Monad[F],T],
  mvhService: MVHService[F,Monad[F],T],
  queryService: QueryService.DataOps[F,Monad[F],T]
)(
  connector: Connector[F,Monad[F]]
)(
  implicit patientSetter: (T,Patient) => T
)
{

  import Orchestrator._
  import UsageScope.{
    MVGenomSeq,
    Research
  }
  import ValidationService.{
    Validate,
    DataValid,
    DataAcceptableWithIssues
  }
  import Completer.syntax._
  import Deidentifier.syntax._
  import Error.ops._

  // PatientRecord Deidentifier:
  // - Remove MV-specific element Patient.address in PatientRecords transferred into the Query module
  private implicit lazy val recordDeidentifier: Deidentifier.Of[T] =
    (record: T) =>
       patientSetter(record,record.patient.copy(address = None))

  private val nonInitial: Set[Submission.Type.Value] =
    Set(Addition,Correction,FollowUp)


  def !(
    cmd: Orchestrator.Command[T]
  )(
    implicit env: Monad[F]
  ): F[EitherNel[Error,Orchestrator.Outcome]] =
    cmd match {

      case Process(rawData,persistValidationOutcome) =>

        for { 

          dataUpload <- rawData.copy(
            // Complete the PatientRecord (resolve display value of Codings etc)
            record = rawData.record.complete,
          )
          .pure // Load pre-processed data into Monad

          validationResult <- validationService ! Validate(dataUpload,persistValidationOutcome.getOrElse(true))

          finalResult <- validationResult match {
            
            // Validation (partially) passed
            case Right(validationOutcome) =>

              // Build a List of transactions/operations constituting the "saga" to be orchestrated:
              val transactions = dataUpload.metadata match {

                // MV submission
                case Some(metadata) =>
                  (mvhService ! MVHService.Process(dataUpload.record,metadata)).adaptErrors :: (

                    // If ResearchConsent is given, save the data in the query module, except for submission type 'test'
                    if (metadata.researchConsents.map(BroadConsent.permitsResearchUse).exists(_ == true) && metadata.`type` != Test)
                      Some(queryService ! QueryService.Save(dataUpload.record.deidentified)).map(_.adaptError)
                      
                    // Else for non-initial submissions, delete data from the query module (in case it has been saved on initial submission)
                    else if (nonInitial(metadata.`type`)) 
                      Some(queryService ! QueryService.Delete(dataUpload.record.id)).map(_.adaptError)

                    else None  // Nothing to do
                    
                  )                                  
                  .toList

                // No MV metadata ("DNPM-only"): Only save in query module
                case None => List(queryService ! QueryService.Save(dataUpload.record.deidentified)).map(_.adaptError)
              }

              for {

                // Execute the "saga" 
                results <- transactions.sequence

                errors = results.flatMap {
                  case Left(errs) => errs.toList
                  case Right(_) => None
                }

                outcome = NonEmptyList.fromList(errors) match {
                  case None => validationOutcome match {
                    case DataValid(data)                       => Saved.asRight
                    case DataAcceptableWithIssues(data,report) => SavedWithIssues(report).asRight
                    // Can't occur but required for exhaustive pattern match
                    case ValidationService.Deleted(_) => NonEmptyList.one(Error(ValidationService.GenericError("Unexpected validation outcome"))).asLeft
                  }

                  case Some(errorNel) => errorNel.asLeft
                }
              
              } yield outcome

            // Validation failed
            case Left(err) => NonEmptyList.one(Error(err)).asLeft.pure 

          }

        } yield finalResult


      case Orchestrator.Delete(patientId,optScopes) =>

        val scopes = optScopes match { 
          case Some(scopes) if scopes.nonEmpty => scopes

          // In order to retain the same behaviour as previously, i.e. to delete the patient's data from every module/scope
          // default to all scopes when none is specified...
          case _ => UsageScope.values.toSet
        }

        for {
          results <- scopes.toList.traverse {
            case MVGenomSeq => (mvhService ! MVHService.DeleteAll(patientId)).adaptErrors
            case Research   => (queryService ! QueryService.Delete(patientId)).adaptError
          }

          // ... and delete from the validation service by default
          validation <- (validationService ! ValidationService.Delete(patientId)).adaptError

          errors = (validation :: results).flatMap {
            case Left(errs) => errs.toList
            case Right(_) => None
          }

          outcome = NonEmptyList.fromList(errors) match {
            case None           => Deleted(patientId).asRight
            case Some(errorNel) => errorNel.asLeft
          }
              
        } yield outcome
    }


  def process(
    request: LocalControllingInfo.Request
  )(
    implicit env: Monad[F]
  ): F[request.ResultType] =
    localControllingInfo(request.criteria)


  private[service] def localControllingInfo(
    criteria: Option[Controlling.Criteria]
  )(
    implicit env: Monad[F]
  ): F[LocalControllingInfo] =
    (
      mvhService.patientDataCounts(criteria),
      queryService.patientDataCounts(criteria)
    )
    .mapN(
      (mvh,query) => LocalControllingInfo(
        Site.local,
        LocalDateTime.now,
        mvh,
        query
      )
    )


  def federatedControllingInfo(
    criteria: Option[Controlling.Criteria],
    sites: Option[Set[Coding[Site]]]
  )(
    implicit env: Monad[F]
  ): F[EitherNel[String,FederatedControllingInfo]] = { 

    val targetSites =
      sites.filter(_.nonEmpty)
        .getOrElse(connector.otherSites + Site.local)

    val localResult =
      if (targetSites contains Site.local)
        for { local <- localControllingInfo(criteria) } yield Some(local) 
      else None.pure

    val externalResults =
      (targetSites - Site.local) match { 
        case sites if sites.nonEmpty =>
          (connector ! (LocalControllingInfo.Request(Site.local,criteria), sites)).map(
            _.map {
              case (site,result) =>
                result.bimap(
                  err => s"Site ${site.code}: $err",
                  List(_)
                )
                .toIor.toIorNel
            }
            .reduceOption(_ combine _)
          )

        case _ => None.pure
      }

    lazy val orderedSites = targetSites.toList.sortBy(_.code.value)

    (
      localResult,
      externalResults
    )
    .mapN(
      (localInfo,external) => external match {
        case Some(externalIor) =>
          externalIor match { 
            case Ior.Right(externalInfos) =>
              NonEmptyList.fromList((externalInfos ++ localInfo).sortBy(_.site.code.value)) -> None

            case Ior.Both(errors,externalInfos) =>
              NonEmptyList.fromList((externalInfos ++ localInfo).sortBy(_.site.code.value)) -> Some(errors)

            case Ior.Left(errors) =>
              localInfo.map(NonEmptyList.of(_)) -> Some(errors)
          }

        case None => localInfo.map(NonEmptyList.of(_)) -> None
      }
    )
    .map {
      case Some(components) -> errors =>
        FederatedControllingInfo(
          LocalDateTime.now,
          orderedSites,
          criteria,
          components.map(_.mvGenomSeqCounts).reduce,
          components.map(_.queryCounts).reduce,
          components,
          errors
        )
        .asRight

      case None -> Some(errors) => errors.asLeft

      case None -> None => "No LocalControllingInfo components available".asLeft.toEitherNel

    }

  }

}
