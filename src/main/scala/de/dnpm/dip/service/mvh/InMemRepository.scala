package de.dnpm.dip.service.mvh


import scala.collection.concurrent.{
  Map,
  TrieMap
}
import cats.Monad
import cats.data.NonEmptyList
import cats.syntax.applicative._
import cats.syntax.either._
import de.dnpm.dip.model.{
  History,
  Id,
  Patient,
  PatientRecord,
}



class InMemRepository[F[_],T <: PatientRecord] extends Repository[F,Monad[F],T]
{

  type Env = Monad[F]


  private val reports: Map[Id[Patient],Map[Id[TransferTAN],Submission.Report]] =
    TrieMap.empty

  private val submissions: Map[Id[Patient],Map[Id[TransferTAN],Submission[T]]] =
    TrieMap.empty


  override def alreadyUsed(id: Id[TransferTAN])(
    implicit env: Env
  ): F[Boolean] =
    reports.exists(_._2.contains(id)).pure


  override def save(
    report: Submission.Report,
    submission: Submission[T]
  )(
    implicit env: Env
  ): F[Either[String,Unit]] = {

    val patId = submission.record.id
    val tan = report.id

    reports.updateWith(patId){ 
      case None          => Some(TrieMap(tan -> report))
      case Some(reports) => Some(reports += tan -> report)
    }

    submissions.updateWith(patId){ 
      case None       => Some(TrieMap(tan -> submission))
      case Some(subs) => Some(subs += tan -> submission)
    }

    ().asRight[String].pure
  }


  override def ?(
    id: Id[TransferTAN]
  )(
    implicit env: Env
  ): F[Option[Submission.Report]] =
    reports.collectFirst {
      case (_,rs) if rs contains id => rs
    }
    .flatMap(_ get id)
    .pure


  override def update(
    report: Submission.Report,
  )(
    implicit env: Env
  ): F[Either[String,Unit]] =
    env.pure(
      reports.get(report.patient) match {
        case Some(rs) =>
          rs.replace(report.id,report)
            .toRight(s"Update failed: Report with TAN ${report.id} doesn't exist")
            .map(_ => ())

        case None => s"Update failed: Report with TAN ${report.id} doesn't exist".asLeft
      }
   )


  override def ?(fltr: Submission.Report.Filter)(
    implicit env: Env
  ): F[Seq[Submission.Report]] =
    reports.values
      .flatMap(_.values)
      .filter(fltr)
      .toSeq
      .pure


  override def ?(fltr: Submission.Filter)(
    implicit env: Env
  ): F[Seq[Submission[T]]] =
    submissions.values
      .flatMap(_.values)
      .filter(fltr)
      .toSeq
      .pure


  override def submissionHistory(id: Id[Patient])(
    implicit env: Env
  ): F[Option[History[Submission[T]]]] =
    submissions.get(id)
      .map(_.values.toList)
      .flatMap(NonEmptyList.fromList(_))
      .map(History(_))
      .pure


  override def submissionReportHistory(id: Id[Patient])(
    implicit env: Env
  ): F[Option[History[Submission.Report]]] =
    reports.get(id)
      .map(_.values.toList)
      .flatMap(NonEmptyList.fromList(_))
      .map(History(_))
      .pure


  override def delete(id: Id[Patient])(
    implicit env: Env
  ): F[Either[String,Unit]] = {
    reports -= id
    submissions -= id

    ().asRight[String].pure
  }

}
