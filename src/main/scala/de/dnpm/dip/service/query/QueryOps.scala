package de.dnpm.dip.service.query


import java.net.URI
import scala.util.Either
import cats.data.{
  Ior,
  IorNel,
  NonEmptyList
}
import de.dnpm.dip.coding.Coding
import de.dnpm.dip.model.{
  Id,
  Patient,
  Snapshot,
  Site
}


/*
final case class QueryFilter
(
  id: Option[Query.Id],
  querier: Option[Querier],
  mode: Option[Set[Coding[Query.Mode.Value]]]
)
*/


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


  import cats.syntax.functor._
  import scala.util.chaining._


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


  // For Admin purposes
  def queries(
    implicit
    env: Env,
    querier: Querier
  ): F[Seq[Query[Criteria,Filters]]]


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


  def patientMatches(
    id: Query.Id,
    filter: PatientFilter,
  )(
    implicit
    env: Env,
    querier: Querier,
    func: cats.Functor[F],
  ): F[Option[Seq[PatientMatch[Criteria]]]] =
    self.resultSet(id)
      .map(
        _.map(
          _.patientMatches(filter)
           .asInstanceOf[Seq[PatientMatch[Criteria]]]  //TODO: Look for type-safe way to handle compile problems with Criteria vs. Results#Criteria
        )
      )

  def patientRecord(
    id: Query.Id,
    patId: Id[Patient]
  )(
    implicit
    env: Env,
    querier: Querier
  ): F[Option[PatientRecord]]


  def retrievePatientRecord(
    site: Coding[Site],
    patient: Id[Patient],
    snapshot: Option[Long] = None
  )(
    implicit
    env: Env,
    querier: Querier
  ): F[Either[Error,Snapshot[PatientRecord]]]



  // Peer-to-peer Ops
  def !(
    req: PeerToPeerQuery[Criteria,PatientRecord]
  )(
    implicit
    env: Env
  ): F[Either[Error,Seq[(Snapshot[PatientRecord],Criteria)]]]


  def !(
    req: PatientRecordRequest[PatientRecord]
  )(
    implicit
    env: Env
  ): F[Option[req.ResultType]]


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

