package de.dnpm.dip.service.query


import scala.util.Either
import de.dnpm.dip.model.{
  Id,
  Patient,
  PatientRecord,
  Snapshot
}
import QueryService.{
  Saved,
  Deleted
}
import de.dnpm.dip.service.controlling.Controlling



trait LocalDB[F[_], Env, Criteria, T <: PatientRecord] extends Controlling.Ops[F,Env]
{

  def save(
    dataSet: T
  )(
    implicit env: Env
  ): F[Either[String,Saved[T]]]


  def delete(
    patient: Id[Patient],
  )(
    implicit env: Env
  ): F[Either[String,Deleted]]


  def ?(
    criteria: Option[Criteria]
  )(
    implicit env: Env
  ): F[Either[String,Seq[Query.Match[T,Criteria]]]]


  def ?(
    patient: Id[Patient],
    snapshot: Option[Long] = None
  )(
    implicit env: Env
  ): F[Option[Snapshot[T]]]

}
