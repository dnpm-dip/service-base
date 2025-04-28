package de.dnpm.dip.service.mvh


import java.time.LocalDateTime
import cats.Monad
import de.dnpm.dip.util.Logging
import de.dnpm.dip.model.PatientRecord


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


  override def !(cmd: Command[T])(
    implicit env: Env
  ): F[Either[Error,Outcome]] =
    cmd match {

      case Process(record,metadata,qcPassed) =>
        log.info(s"Processing MVH submission for Patient record ${record.id}")

        val datetime = LocalDateTime.now

        repo.save(
          Submission.Report(
            datetime,
            useCase,
            metadata.`type`,
            metadata.transferTAN,
            qcPassed
          ),
          Submission(record,metadata,datetime)
        )
        .map(
          _.bimap(
            GenericError(_),
            _ => Saved
          )
        )

/*
      case Process(record,metadata) =>
        log.info(s"Processing MVH submission for Patient record ${record.id}")
        repo.save(
          Submission(record,metadata,LocalDateTime.now)
        )
        .map(
          _.bimap(
            GenericError(_),
            _ => Saved
          )
        )
*/
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

  override def ?(filter: Submission.Filter)(
    implicit env: Env
  ): F[Iterable[Submission[T]]] =
    repo ? filter

}
