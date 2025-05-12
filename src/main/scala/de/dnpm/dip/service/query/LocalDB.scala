package de.dnpm.dip.service.query


import scala.util.Either
import de.dnpm.dip.model.{
  Id,
  Patient,
  Snapshot
}
import QueryService.{
  Saved,
  Deleted
}


trait LocalDB[
  F[_],
  Env,
  Criteria,
  PatientRecord
]{

  def save(
    dataSet: PatientRecord
  )(
    implicit env: Env
  ): F[Either[String,Saved[PatientRecord]]]


  def delete(
    patient: Id[Patient],
  )(
    implicit env: Env
  ): F[Either[String,Deleted]]


  def ?(
    criteria: Option[Criteria]
  )(
    implicit env: Env
  ): F[Either[String,Seq[Query.Match[PatientRecord,Criteria]]]]


  def ?(
    patient: Id[Patient],
    snapshot: Option[Long] = None
  )(
    implicit env: Env
  ): F[Option[Snapshot[PatientRecord]]]


  def totalRecords(
    implicit env: Env
  ): F[Int]

}
