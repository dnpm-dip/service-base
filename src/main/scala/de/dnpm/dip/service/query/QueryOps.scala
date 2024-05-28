package de.dnpm.dip.service.query



import java.net.URI
import scala.util.Either
import cats.Functor
import cats.data.{
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
import de.dnpm.dip.service.Data


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
  type Filter        = UseCase#Filter
  type Results       = UseCase#Results


  import cats.syntax.functor._
  import scala.util.chaining._


  def sites(
    implicit env: Env
  ): F[Sites]


  def !(
    cmd: Query.Command[Criteria,Filter]
  )(
    implicit
    env: Env,
    querier: Querier
  ): F[Error IorNel Query.Outcome[Criteria,Filter]]
//  ): F[Error IorNel Query[Criteria,Filter]]


  def get(
    id: Query.Id
  )(
    implicit
    env: Env,
    querier: Querier
  ): F[Option[Query[Criteria,Filter]]]


  // For Admin purposes
  def queries(
    implicit
    env: Env,
    querier: Querier
  ): F[Seq[Query[Criteria,Filter]]]

 
  def resultSet(
    id: Query.Id
  )(
    implicit
    env: Env,
    querier: Querier,
  ): F[Option[Results]]


  def summary(
    id: Query.Id,
    filter: Filter,
  )(
    implicit
    env: Env,
    querier: Querier,
  ): F[Option[Results#SummaryType]] 


  def patientMatches(
    id: Query.Id,
    filter: Filter,
  )(
    implicit
    env: Env,
    querier: Querier,
  ): F[Option[Seq[PatientMatch[Criteria]]]]


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
]
extends Data.Ops[F,Env,UseCase#PatientRecord]
with QueryOps[F,Env,UseCase,String]
with PreparedQueryOps[F,Env,UseCase#Criteria,String]

