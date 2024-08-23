package de.dnpm.dip.service.validation


import scala.util.Either
import scala.collection.concurrent.{ 
  Map,
  TrieMap
}
import cats.Monad
import cats.syntax.applicative._
import cats.syntax.either._
import de.dnpm.dip.model.{
  Id,
  Patient
}


class InMemRepository[F[_],PatientRecord] extends Repository[F,Monad[F],PatientRecord]
{

  private val db: Map[Id[Patient],(PatientRecord,ValidationReport)] =
    TrieMap.empty


  override def save(
    data: PatientRecord,
    report: ValidationReport
  )(
    implicit env: Monad[F]
  ): F[Either[String,Unit]] =
    db.update(report.patient,data ->report)
      .asRight[String]
      .pure


  override def ?(
    filter: ValidationService.Filter
  )(
    implicit env: Monad[F]
  ): F[Iterable[(PatientRecord,ValidationReport)]] =
    filter.severities match {

      case Some(severities) =>
        db.values.filter {
          case (_,report) => severities contains report.maxSeverity
        }
        .pure

      case None =>
        db.values.pure
    }


  override def ?(
    id: Id[Patient]
  )(
    implicit env: Monad[F]
  ): F[Option[(PatientRecord,ValidationReport)]] =
    db.get(id)
      .pure
 

  override def delete(
    id: Id[Patient]
  )(
    implicit env: Monad[F]
  ): F[Either[String,Unit]] = {

    db -= id

    ().asRight[String].pure
  }


}
