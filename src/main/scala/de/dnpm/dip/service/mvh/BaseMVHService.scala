package de.dnpm.dip.service.mvh


import java.time.LocalDateTime
import cats.Monad
import de.dnpm.dip.util.Logging
import de.dnpm.dip.service.Distribution
import de.dnpm.dip.model.{
  HealthInsurance,
  NGSReport,
  PatientRecord,
  Site
}


class BaseMVHService[F[_],T <: PatientRecord](
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


  type Env = Monad[F]


  import NGSReport.Type._


  private val ngsTypeOrdering: Ordering[NGSReport.Type.Value] =
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
                record.ngsReports.flatMap(
                  _.collect {
                    case ngs if ngs.variants.nonEmpty => NGSReport.Type.unapply(ngs.`type`).get  // .get safe here
                  }
                  .maxOption(ngsTypeOrdering)
                ),
                HealthInsurance.Type.unapply(record.patient.healthInsurance.`type`).get // .get safe here
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


  override def statusInfo(
    implicit env: Env
  ): F[StatusInfo] =
    (repo ? Submission.Report.Filter())
      .map(_.map(_.status))
      .map(Distribution.of(_))
      .map(MVHService.StatusInfo(_))

}
