package de.dnpm.dip.service.query


import scala.util.Either
import de.dnpm.dip.model.{
  Id,
  Patient,
  Snapshot,
  Site
}


trait LocalDB[
  F[_],
  Env,
  Parameters,
  PatientRecord
]{

  def save(
    dataSet: PatientRecord
  )(
    implicit env: Env
  ): F[Either[String,Data.Saved[PatientRecord]]]


  def delete(
    patient: Id[Patient],
  )(
    implicit env: Env
  ): F[Either[String,Data.Deleted]]


  def ?(
    parameters: Parameters
  )(
    implicit env: Env
  ): F[Either[String,Seq[Snapshot[PatientRecord]]]]


  def ?(
    patient: Id[Patient],
    snapshot: Option[Id[Snapshot[PatientRecord]]] = None
  ): F[Option[Snapshot[PatientRecord]]]

}


