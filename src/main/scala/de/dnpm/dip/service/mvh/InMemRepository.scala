package de.dnpm.dip.service.mvh


import scala.collection.concurrent.{
  Map,
  TrieMap
}
import cats.Monad
import cats.syntax.applicative._
import cats.syntax.either._
import de.dnpm.dip.model.{
  Id,
  Patient,
  PatientRecord,
}



class InMemRepository[F[_],T <: PatientRecord] extends Repository[F,Monad[F],T]
{
  type Env = Monad[F]


  private val reports: Map[Id[Patient],Submission.Report] =
    TrieMap.empty

  private val submissions: Map[Id[Patient],Submission[T]] =
    TrieMap.empty


  override def save(
    report: Submission.Report,
    submission: Submission[T]
  )(
    implicit env: Env
  ): F[Either[String,Unit]] = {

    reports += submission.record.id -> report
    submissions += submission.record.id -> submission

    ().asRight[String].pure
  }


  override def ?(
    id: Id[TransferTAN]
  )(
    implicit env: Env
  ): F[Option[Submission.Report]] =
    reports.collectFirst {
      case (_,r) if r.id == id => r 
    }
    .pure


  override def update(
    report: Submission.Report,
  )(
    implicit env: Env
  ): F[Either[String,Unit]] =
    env.pure(
      reports.collectFirst { case (patId,r) if r.id == report.id => patId } match {
        case Some(patId) => reports.update(patId,report).asRight
        case None        => s"Update failed: Report with TAN ${report.id} doesn't exist".asLeft
      }
   )

  override def ?(fltr: Submission.Report.Filter)(
    implicit env: Env
  ): F[Iterable[Submission.Report]] =
    reports.values
      .filter(fltr)
      .pure

  override def ?(fltr: Submission.Filter)(
    implicit env: Env
  ): F[Iterable[Submission[T]]] =
    submissions.values
      .filter(fltr)
      .pure


  override def delete(id: Id[Patient])(
    implicit env: Env
  ): F[Either[String,Unit]] = {
    reports -= id
    submissions -= id

    ().asRight[String].pure
  }

}
