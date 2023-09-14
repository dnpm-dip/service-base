package de.dnpm.dip.service.query


import java.net.URI
import scala.util.Either
import cats.data.{Ior,IorNel,NonEmptyList}
import de.dnpm.dip.coding.Coding
import de.dnpm.dip.model.{
  Id,
  Patient,
  Snapshot,
  Site
}


trait QueryOps[
  F[+_],
  Env,
  UseCase <: UseCaseConfig,
  Error
]
{
  self =>

  type PatientRecord = UseCase#PatientRecord
  type Criteria      = UseCase#Criteria
  type Filters       = UseCase#Filters
  type Results       = UseCase#Results



  def sites: List[Coding[Site]]


  def process(
    cmd: Query.Command[Criteria,Filters]
  )(
    implicit
    env: Env,
    querier: Querier
  ): F[Error IorNel Query[Criteria,Filters]]

  final def !(
    cmd: Query.Command[Criteria,Filters]
  )(
    implicit
    env: Env,
    querier: Querier
  ): F[Error IorNel Query[Criteria,Filters]] = self.process(cmd)


  def get(
    id: Query.Id
  )(
    implicit
    env: Env,
    querier: Querier
  ): F[Option[Query[Criteria,Filters]]]


  def summary(
    id: Query.Id
  )(
    implicit
    env: Env,
    querier: Querier
  ): F[Option[Results#Summary]]

  
  def resultSet(
    id: Query.Id
  )(
    implicit
    env: Env,
    querier: Querier
  ): F[Option[Results]]


  def patientRecord(
    id: Query.Id,
    patId: Id[Patient]
  )(
    implicit
    env: Env,
    querier: Querier
  ): F[Option[PatientRecord]]


  def !(
    req: PeerToPeerQuery[Criteria,PatientRecord]
  )(
    implicit
    env: Env
  ): F[Either[Error,Seq[(Snapshot[PatientRecord],Criteria)]]]


  def fetchPatientRecord(
    site: Coding[Site],
    patient: Id[Patient],
    snapshot: Option[Long] = None
  )(
    implicit
    env: Env,
    querier: Querier
  ): F[Either[Error,Snapshot[PatientRecord]]]


}


trait QueryService[
  F[+_],
  Env,
  UseCase <: UseCaseConfig,
  Error
]
extends Data.Ops[
  F,Env,UseCase,Error
]
with QueryOps[
  F,Env,UseCase,Error
]

