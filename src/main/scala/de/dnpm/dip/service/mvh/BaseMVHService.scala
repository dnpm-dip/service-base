package de.dnpm.dip.service.mvh


import java.time.{
  LocalTime,
  LocalDateTime
}
import cats.Monad
import de.dnpm.dip.util.Logging
import de.dnpm.dip.service.Distribution
import de.dnpm.dip.model.{
  NGSReport,
  PatientRecord,
  Site
}
import ResearchConsent.{
  MDAT_RESEARCH_USE,
  PATDAT_STORE_AND_USE
}


abstract class BaseMVHService[F[_],T <: PatientRecord](
  override val useCase: UseCase.Value,
  val repo: Repository[F,Monad[F],T]
)
extends MVHService[F,Monad[F],T] 
with Logging
{
  import MVHService._
  import cats.syntax.either._
  import cats.syntax.functor._
  import cats.syntax.flatMap._
  import Submission.Report.Status._
  import extensions._
  import NGSReport.Type._


  type Env = Monad[F]


  private implicit val ngsTypeOrdering: Ordering[NGSReport.Type.Value] =
    Ordering.by {
      case GenomeLongRead  => 4
      case GenomeShortRead => 3
      case Exome           => 2
      case Panel           => 1
      case _               => 0
    }

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
                record.mvhSequencingReports.map[NGSReport.Type.Value](_.`type`.code).maxOption,
                record.patient.healthInsurance.`type`.code,
                Some(
                  Map(
                    Consent.Category.ModelProject ->
                      metadata.modelProjectConsent
                        .provisions
                        .exists(p => p.purpose == ModelProjectConsent.Purpose.Sequencing && p.`type` == Consent.Provision.Type.Permit),
                    Consent.Category.Research ->
                      metadata.researchConsents
                        .filter(_.nonEmpty)
                        .exists(_.forall(consent => consent.permits(PATDAT_STORE_AND_USE) || consent.permits(MDAT_RESEARCH_USE)))
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
                  GenericError(s"Invalid TTAN $id").asLeft
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


  override def ?(filter: Submission.Filter)(
    implicit env: Env
  ): F[Seq[Submission[T]]] =
    repo ? filter


  protected def baseReport(
    criteria: Report.Criteria
  )(
    implicit env: Env
  ): F[BaseReport] = {

    val (quarter,period) = criteria match {
      case Report.ForQuarter(n,year) => Some(n) -> Report.Quarter(n,year)
      case Report.ForPeriod(period)  => None -> period 
    }

    for {
      submissions <- repo ? Submission.Filter(
        period.copy(
          start = period.start.atTime(LocalTime.MIN),
          end   = period.start.atTime(LocalTime.MIDNIGHT),
        )
      )

      metadata  = submissions.map(_.metadata)

    } yield BaseReport(
       Site.local,
       LocalDateTime.now,
       quarter,
       period,
       useCase,
       Distribution.of(metadata.map(_.`type`)),
       None   // TODO
    )
  }


/*
  override def statusInfo(
    implicit env: Env
  ): F[StatusInfo] =
    (repo ? Submission.Report.Filter())
      .map(_.map(_.status))
      .map(Distribution.of(_))
      .map(MVHService.StatusInfo(_))
*/

}
