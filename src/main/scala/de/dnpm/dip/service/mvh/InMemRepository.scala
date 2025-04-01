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


  private val cache: Map[Id[Patient],MVHPatientRecord[T]] =
    TrieMap.empty


  override def save(
    mvhRecord: MVHPatientRecord[T]
  )(
    implicit env: Env
  ): F[Either[String,Unit]] = {

    cache += mvhRecord.record.id -> mvhRecord

    ().asRight[String].pure
  }


  override def ?(fltr: MVHPatientRecord.Filter)(
    implicit env: Env
  ): F[Iterable[MVHPatientRecord[T]]] =
    cache.values
//    .map(_._2)
    .filter(fltr)
    .pure


  override def delete(id: Id[Patient])(
    implicit env: Env
  ): F[Either[String,Unit]] = {
    cache -= id

    ().asRight[String].pure
  }

}
