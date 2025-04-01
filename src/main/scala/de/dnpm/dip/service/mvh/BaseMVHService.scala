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

      case Process(record,metadata) =>
        log.info(s"Processing MVH submission for Patient record ${record.id}")
        repo.save(
          MVHPatientRecord(record,metadata,LocalDateTime.now)
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

  override def ?(filter: MVHPatientRecord.Filter)(
    implicit env: Env
  ): F[Iterable[MVHPatientRecord[T]]] =
    repo ? filter

}
