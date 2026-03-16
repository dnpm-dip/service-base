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
import Submission.Type._
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


 /**
  * Abstract method to project the "diagnostic extent" from the
  * use-case-specific specialization of PatientRecord.
  * This info is added to Submission.Report in order for it to contain all info necessary for 
  * compilation of quarter report (which include info on number of single, duo, trio cases
  */
  protected def diagnosticExtent(record: T): Option[Submission.DiagnosticExtent.Value]

  protected def sequenceTypes(record: T): Option[Set[Submission.SequenceType.Value]]


  override def !(cmd: Command[T])(
    implicit env: Env
  ): F[Either[Error,Outcome]] =
    cmd match {

      case Process(record,metadata) =>

        log.info(s"Processing MVH submission for Patient record ${record.id}")

        for {
          tanAlreadyUsed <- repo.alreadyUsed(metadata.transferTAN)

          // Check acceptability of submission type for the given Patient
          submissionTypeOk <- metadata.`type` match {

            // An 'initial' submission must be the first at all or, in case of a re-inclusion into MV,
            // given that at most one 'initial' submission is allowed per episode of care (i.e. "Fall"),
            // ensure there are more episodes of care that 'initial' submissions
            case Initial =>
              repo.submissionReportHistory(record.patient.id).map {
                case None => true

                case Some(submissions) => 
                  record.episodesOfCare.size > submissions.history.toList.count(_.`type` == Initial)
              }

            // 'addition', 'correction', 'followup' can only be appended to an existing submission history contaning
            // 'initial' submission for the latest/current episode of care
            case Addition | Correction | FollowUp =>
              repo.submissionReportHistory(record.patient.id).map {
                case Some(submissions) =>

                  val currentEpisodeOfCare = record.episodesOfCare.toList.maxBy(_.period.start)

                  submissions.history.exists(
                    sub => sub.`type` == Initial &&
                      sub.createdAt.isAfter(currentEpisodeOfCare.period.start.atTime(LocalTime.MIN))
                  )

                case None => false
              }

            // Test submission ok any time
            case Test => env.pure(true)
          }

          result <- (!tanAlreadyUsed,submissionTypeOk) match {
            case (true,true) =>

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
                  diagnosticExtent(record),
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

            case (false,_) =>
              val msg = s"Invalid submission: TAN ${metadata.transferTAN} has already been used"
              log.warn(s"$msg, refusing submission")
              env.pure(InvalidTAN(msg).asLeft)

            case (_,false) =>
              val msg = s"Invalid submission: Type ${metadata.`type`} is inconsistent with previous submission history"
              log.warn(s"$msg, refusing submission")
              env.pure(InvalidSubmissionType(msg).asLeft)
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
