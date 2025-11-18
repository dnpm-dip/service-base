package de.dnpm.dip.service.mvh


import java.time.{
  LocalTime,
  LocalDateTime
}
import cats.Monad
import cats.syntax.either._
import cats.syntax.functor._
import cats.syntax.flatMap._
import de.dnpm.dip.util.Logging
import de.dnpm.dip.service.Distribution
import de.dnpm.dip.model.{
  ClosedPeriod,
  Id,
  NGSReport,
  PatientRecord,
  Site
}
import MVHService._
import Submission.Report.Status._
import extensions._
import NGSReport.Type._
import ModelProjectConsent.Purpose.Sequencing 


abstract class BaseMVHService[F[_],T <: PatientRecord](
  override val useCase: UseCase.Value,
  val repo: Repository[F,Monad[F],T]
)
extends MVHService[F,Monad[F],T] 
with Logging
{

  type Env = Monad[F]


  private implicit val ngsTypeOrdering: Ordering[NGSReport.Type.Value] =
    Ordering.by {
      case GenomeLongRead  => 4
      case GenomeShortRead => 3
      case Exome           => 2
      case Panel           => 1
      case _               => 0
    }


  protected def sequenceTypes(record: T): Option[Set[Submission.SequenceType.Value]]


  override def !(cmd: Command[T])(
    implicit env: Env
  ): F[Either[Error,Outcome]] =
    cmd match {

      case Process(record,metadata) =>

        log.info(s"Processing MVH submission for Patient record ${record.id}")

        for {
          tanAlreadyUsed <- repo.alreadyUsed(metadata.transferTAN)

          result <- if (!tanAlreadyUsed){

            val submittedAt = LocalDateTime.now

            repo.save(
              Submission.Report(
                metadata.transferTAN,
                submittedAt,
                record.patient.id,
                Unsubmitted,
                Site.local,
                useCase,
                metadata.`type`,
                record.mvhSequencingReports.map(_.`type`.code.enumValue).maxOption,
                sequenceTypes(record),
                record.patient.healthInsurance.`type`.code,
                Some(
                  Map(
                    Consent.Category.ModelProject ->
                      metadata.modelProjectConsent.provisions.exists(p => p.purpose == Sequencing && p.`type` == Consent.Provision.Type.Permit),
                    Consent.Category.Research ->
                      metadata.researchConsents.exists(BroadConsent.permitsResearchUse)
                  )
                ),
                metadata.reasonResearchConsentMissing
              ),
              Submission(
                record,
                metadata,
                submittedAt
              )
            )
            .map(
              _.bimap(
                GenericError(_),
                _ => Saved
              )
            )
          } else {
            val msg = s"Invalid submission: TAN ${metadata.transferTAN} has already been used"
            log.warn(s"$msg, refusing submission")
            env.pure(InvalidTAN(msg).asLeft)
          }

        } yield result

      case ConfirmSubmitted(id) =>
        for {
          optReport <- repo ? id

          result <- 
            optReport match {
              case Some(report) =>
                repo.update(report.copy(status = Submitted))
                  .map(
                    _.bimap(
                      GenericError(_),
                      _ => Updated
                    )
                  )

              case None =>
                env.pure(
                  GenericError(s"Invalid TAN $id").asLeft
                )
            }

        } yield result


      case Delete(id) =>
        log.info(s"Deleting MVH data for Patient $id")
        repo.delete(id)
          .map(
            _.bimap(
              GenericError(_),
              _ => Deleted
            )
          )
    }


  override def ?(filter: Submission.Report.Filter)(
    implicit env: Env
  ): F[Seq[Submission.Report]] =
    repo ? filter


  override def ?(id: Id[TransferTAN])(
    implicit env: Env
  ): F[Option[Submission.Report]] =
    repo ? id


  override def ?(filter: Submission.Filter)(
    implicit env: Env
  ): F[Seq[Submission[T]]] =
    repo ? filter


  // Create BaseReport for the criteria and return it together with the submissions it's based on,
  // in case the implementing subclass needs to extract additional info from the submissions
  protected def baseReport(
    criteria: Report.Criteria
  )(
    implicit env: Env
  ): F[(BaseReport,Seq[Submission[T]])] = {

    log.info(s"Creating MVH Report: $criteria")

    val (quarter,period) = criteria match {
      case Report.ForQuarter(n,year)   => Some(n) -> Report.Quarter(n,year)
      case Report.ForPeriod(start,end) => None -> ClosedPeriod(start,end)
    }

    for {
      submissions <- repo ? Submission.Filter(
        period.copy(
          start = period.start.atTime(LocalTime.MIN),
          end   = period.end.atTime(LocalTime.MIDNIGHT),
        )
      )

      submissionTypes = submissions.map(_.metadata.`type`)

      report = BaseReport(
        Site.local,
        LocalDateTime.now,
        quarter,
        period,
        useCase,
        Distribution.of(submissionTypes),
        None   // TODO
      )

    } yield report -> submissions
  }


  override def statusInfo(
    implicit env: Env
  ): F[StatusInfo] =
    (repo ? Submission.Report.Filter())
      .map(_.map(_.status))
      .map(Distribution.of(_))
      .map(MVHService.StatusInfo(_))

}
