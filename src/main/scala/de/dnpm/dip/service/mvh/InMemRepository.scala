package de.dnpm.dip.service.mvh


//import scala.util.chaining._
import scala.collection.concurrent.{
  Map,
  TrieMap
}
import cats.Monad
import cats.syntax.applicative._
import cats.syntax.either._
import play.api.libs.json.{
  Json,
  Format,
  Reads,
  Writes
}
import de.dnpm.dip.model.{
  Id,
  Patient,
  PatientRecord,
}



class InMemRepository[F[_],T <: PatientRecord]
extends Repository[F,Monad[F],T]
{
  type Env = Monad[F]


  private val cache: Map[Id[Patient],(MVHPatientRecord[T],SubmissionReport)] =
    TrieMap.empty


  override def save(
    mvhRecord: MVHPatientRecord[T],
    report: SubmissionReport
  )(
    implicit env: Env
  ): F[Either[String,Unit]] = {

    cache += mvhRecord.record.id -> (mvhRecord,report)

    ().asRight[String].pure
  }


  override def ?(fltr: SubmissionReport.Filter)(
    implicit env: Env
  ): F[Iterable[SubmissionReport]] =
    cache.values
    .map(_._2)
    .filter(fltr)
    .pure


  override def delete(id: Id[Patient])(
    implicit env: Env
  ): F[Either[String,Unit]] = {
    cache -= id

    ().asRight[String].pure
  }

}
