package de.dnpm.dip.service.mvh


import java.time.LocalDateTime
import scala.util.{
  Left,
  Right
}
import cats.Monad
import de.dnpm.dip.util.Logging
import de.dnpm.dip.model.{
  Id,
  Patient,
  PatientRecord,
  Site
}
import de.dnpm.dip.model.NGSReport.SequencingType


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

  type Env = Monad[F]


  private def submissionReport(
    record: T,
    meta: Metadata,
    qcPassed: Boolean
  ): SubmissionReport =
    SubmissionReport(
      LocalDateTime.now,
      Site.local,
      useCase,
      meta.transferTAN,
      meta.submissionType,
      record
        .ngsReports
        .getOrElse(List.empty)
        .map(_.sequencingType)
        .collect { case SequencingType(st) => st }
        .maxOption,
      qcPassed
    )


  override def !(cmd: Command[T])(
    implicit env: Env
  ): F[Either[Error,Outcome]] =
    cmd match {

      case Process(mvhRecord @ MVHPatientRecord(record,metadata),qcPassed) =>
        log.info(s"Processing MVH submission for Patient record ${record.id}")
        repo.save(
          mvhRecord,
          submissionReport(record,metadata,qcPassed)
        )
        .map(
          _.bimap(
            GenericError(_),
            _ => Saved
          )
        )

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


  override def ?(filter: SubmissionReport.Filter)(
    implicit env: Env
  ): F[Iterable[SubmissionReport]] =
    repo ? filter

}
