package de.dnpm.dip.service.mvh


import java.time.{
  LocalTime,
  LocalDateTime
}
import cats.Monad
import cats.data.NonEmptyList
import cats.syntax.either._
import cats.syntax.functor._
import cats.syntax.flatMap._
import de.dnpm.dip.util.Logging
import de.dnpm.dip.service.Distribution
import de.dnpm.dip.model.{
  ClosedPeriod,
  EpisodeOfCare,
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


abstract class BaseMVHService[F[+_],T <: PatientRecord](
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


  /**
   * Check whether an 'initial' Submission.Report exists for the given EpisodeOfCare.
   *
   * Given that the EpisodeOfCare reference on Submission.Metadata is optional (for backwards compatibility)
   * and may thus not be defined, fallback logic is required:
   * If defined, check whether an 'initial' submission exists for this Id[EpisodeOfCare]
   *
   * Else assume the Submission to pertain to the current, i.e. latest declared EpisodeOfCare and
   * check whether and 'initial' submissions exists after this EpisodeOfCare's start date 
   */
  private def initialSubmissionExistsForEpisode(
    reports: NonEmptyList[Submission.Report],
    episode: Option[EpisodeOfCare],
    record: T
  ): Boolean =
    episode match {
      case Some(episode) =>
        reports.exists(sub => sub.episodeOfCare.exists(_ == episode.id) && sub.`type` == Initial)

      case None => 
        val episodeStart =  record.currentEpisodeOfCare.period.start.atTime(LocalTime.MIN)
        reports.exists(sub => sub.createdAt.isAfter(episodeStart) && sub.`type` == Initial)
    }



  override def !(cmd: Command[T])(
    implicit env: Env
  ): F[Either[Error,Outcome]] =
    cmd match {

      case Process(record,metadata) =>

        log.info(s"Processing MVH submission for Patient record ${record.id}")

        for {

          optTanError <- repo.alreadyUsed(metadata.transferTAN).map {
            case true =>
              val msg = s"Invalid submission: TAN ${metadata.transferTAN} has already been used"
              log.warn(s"$msg, refusing submission")
              Some(InvalidTAN(msg))

            case false => None
          }

          processingResult <- optTanError match {

            // Fail-fast in case of TAN error
            case Some(tanError) => env.pure(tanError.asLeft)

            case None =>
              for {

                priorSubmissionReports <- repo.submissionReportHistory(record.patient.id).map(_.map(_.history))

                episode = metadata.episodeOfCare.flatMap(_ resolveOn record.episodesOfCare)

                // Check acceptability of submission type for the given Patient
                optSubmissionTypeError = metadata.`type` match {

                  // An Initial submission must be the first at all for the current EpisodeOfCare,
                  // so report an error if one already exists, or for additional safety given the newly introduced
                  // reference Submission.Metadata.episodeOfCare, there are not more EpisodesOfCare than 'initial' submissions
                  case Initial =>
                    priorSubmissionReports match {
                      case Some(reports) if initialSubmissionExistsForEpisode(reports,episode,record) || (record.episodesOfCare.size <= reports.toList.count(_.`type` == Initial)) =>
                        Some(InvalidSubmissionType(s"Invalid submission: only one ${Initial} submission allowed per EpisodeOfCare"))

                      case _ => None
                    }
      
                  // Conversely, Addition, Correction, FollowUp can only be appended if an initial submission exists in the current EpisodeOfCare
                  case Addition | Correction | FollowUp =>
                    priorSubmissionReports match {
                      case Some(reports) if initialSubmissionExistsForEpisode(reports,episode,record) =>
                        None

                      case _ =>
                        Some(InvalidSubmissionType(s"Invalid submission: type ${metadata.`type`} can only occur after an ${Initial} submission for the current EpisodeOfCare"))
                    }

                  // Test submission ok any time
                  case Test => None

                }
      
                result <- optSubmissionTypeError match {

                  case None => 
                    processSubmission(
                      record,
                      metadata,
                      priorSubmissionReports
                        .map(_.filterNot(_.`type` == Test))  // Exclude 'test' SubmissionReports from processing (consent revocation check)
                        .flatMap(_.maxByOption(_.createdAt))
                    )

                  case Some(error) => 
                    log.warn(s"${error.msg}, refusing submission")
                    env.pure(error.asLeft)
                  }

               } yield result
          }

        } yield processingResult


      case ConfirmSubmitted(id) =>
        for {
          optReport <- repo submissionReport id

          result <- optReport match {
            case Some(report) =>
              repo.update(report.copy(status = Submitted))
                .map(
                  _.bimap(
                    GenericError(_),
                    _ => Updated
                  )
                )

            case None =>
              env.pure(GenericError(s"Invalid TAN $id").asLeft)
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


    private def processSubmission(
      record: T,
      metadata: Submission.Metadata,
      previousSubmissionReport: Option[Submission.Report]
    )(
      implicit env: Monad[F]
    ): F[Either[Error,Saved.type]] = {

      val submittedAt = LocalDateTime.now

      val consentStatus =
        Map(
          Consent.Category.ModelProject ->
            metadata.modelProjectConsent.provisions.exists(p => p.purpose == Sequencing && p.`type` == Consent.Provision.Type.Permit),
          Consent.Category.Research ->
            metadata.researchConsents.exists(BroadConsent.permitsResearchUse)
        )

      // Check for revocation of consent by comparing each Consent.Category status between the previous and current submission:
      // Change of status from true -> false is interpreted as revocation
      val consentRevocation =
        for {
          previousConsentStatus <- previousSubmissionReport.flatMap(_.consentStatus)
        } yield Consent.Category.values.map {
          category => 
            val revoked = (previousConsentStatus.getOrElse(category,false), consentStatus(category)) match {
              case (true,false) => true
              case _            => false
            }
            category -> revoked
        }
        .toMap

      repo.save(
        Submission.Report(
          metadata.transferTAN,
          submittedAt,
          record.patient.id,
          metadata.episodeOfCare.map(_.id).orElse(Some(record.currentEpisodeOfCare.id)),
          Unsubmitted,
          Site.local,
          useCase,
          metadata.`type`,
          record.mvhSequencingReports.map(_.`type`.code.enumValue).maxOption,
          diagnosticExtent(record),
          sequenceTypes(record),
          record.patient.healthInsurance.`type`.code,
          Some(consentStatus),
          consentRevocation,
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

    }


  override def ?(filter: Submission.Report.Filter)(
    implicit env: Env
  ): F[Seq[Submission.Report]] =
    repo ? filter


  override def submissionReport(id: Id[TransferTAN])(
    implicit env: Env
  ): F[Option[Submission.Report]] =
    repo submissionReport id


  override def ?(filter: Submission.Filter)(
    implicit env: Env
  ): F[Seq[Submission[T]]] =
    repo ? filter

  override def submission(id: Id[TransferTAN])(
    implicit env: Env
  ): F[Option[Submission[T]]] =
    repo submission id


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
        period = Some(
          period.copy(
            start = period.start.atTime(LocalTime.MIN),
            end   = period.end.atTime(LocalTime.MIDNIGHT),
          )
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
