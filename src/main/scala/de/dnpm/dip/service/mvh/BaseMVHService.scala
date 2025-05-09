package de.dnpm.dip.service.mvh


import java.time.LocalDateTime
import cats.Monad
import de.dnpm.dip.util.Logging
import de.dnpm.dip.model.{
  History,
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


  override def !(cmd: Command[T])(
    implicit env: Env
  ): F[Either[Error,Outcome]] =
    cmd match {

      case Process(record,metadata) =>
        log.info(s"Processing MVH submission for Patient record ${record.id}")

        val datetime = LocalDateTime.now

        repo.save(
          Submission.Report(
            metadata.transferTAN,
            datetime,
            record.patient.id,
            History(
              Submission.Report.Status(
                Unsubmitted,
                datetime
              )
            ),
            Site.local,
            useCase,
            metadata.`type`,
            record.patient.healthInsurance.`type`
          ),
          Submission(
            record,
            metadata,
            datetime
          )
        )
        .map(
          _.bimap(
            GenericError(_),
            _ => Saved
          )
        )

      case ConfirmSubmitted(id) =>
        for {
          optReport <- repo ? id

          result <- 
            optReport match {
              case Some(report) =>
                repo.update(
                  report.copy(
                    status = report.status :+ Submission.Report.Status(Submitted, LocalDateTime.now)
                  )
                )
                .map(
                  _.bimap(
                    GenericError(_),
                    _ => Updated(id)
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
  ): F[Iterable[Submission.Report]] =
    repo ? filter
/*
   (repo ? filter).map(
     _.tapEach(
       r => repo.update(
         r.copy(
           status = r.status :+ Submission.Report.Status(
              RequestedForProcessing,
              LocalDateTime.now
           )
         )
       )
     )
   )
*/

  override def ?(filter: Submission.Filter)(
    implicit env: Env
  ): F[Iterable[Submission[T]]] =
    repo ? filter

}
