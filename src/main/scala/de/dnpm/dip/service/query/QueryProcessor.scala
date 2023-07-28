package de.dnpm.dip.service.query


import java.net.URI
import scala.util.Either
import scala.concurrent.{Future,ExecutionContext}
import cats.data.{Ior,IorNel,NonEmptyList}
import de.dnpm.dip.coding.Coding
import de.dnpm.dip.model.{
  Id,
  Patient,
  Snapshot,
  Site
}


trait QueryOps[
  F[_],
  Env,
  UseCase <: UseCaseConfig,
  Error
]
{
  self =>

  type PatientRecord = UseCase#PatientRecord
  type Parameters    = UseCase#Parameters
  type Filters       = UseCase#Filters
  type Results       = UseCase#Results

  def useCase: String


  def process(
    cmd: Query.Command[Parameters,Filters]
  )(
    implicit
    env: Env,
    querier: Querier
  ): F[Error IorNel Query[Parameters,Filters]]

  final def !(
    cmd: Query.Command[Parameters,Filters]
  )(
    implicit
    env: Env,
    querier: Querier
  ): F[Error IorNel Query[Parameters,Filters]] = self.process(cmd)


  def get(
    id: Query.Id
  )(
    implicit
    env: Env,
    querier: Querier
  ): F[Option[Query[Parameters,Filters]]]


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



  def !(
    req: PeerToPeerQuery[Parameters,PatientRecord]
  )(
    implicit
    env: Env
  ): F[Either[Error,Seq[Snapshot[PatientRecord]]]]

/*
  def !(
    req: PeerToPeerRequest
  )(
    implicit
    env: Env
  ): F[Either[Error,PeerToPeerResponse[req.ResultType]]]
*/


  def getPatientRecord(
    site: Coding[Site],
    patient: Id[Patient],
    snapshot: Option[Id[Snapshot[PatientRecord]]] = None
  )(
    implicit
    env: Env,
    querier: Querier
  ): F[Either[Error,Snapshot[PatientRecord]]]


}


trait QueryProcessor[
  UseCase <: UseCaseConfig,
  Error
]
extends QueryOps[
  Future,ExecutionContext,UseCase,Error
]

