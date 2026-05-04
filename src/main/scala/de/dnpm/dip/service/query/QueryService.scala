package de.dnpm.dip.service.query


import scala.util.Either
import de.dnpm.dip.coding.Coding
import de.dnpm.dip.model.{
  Id,
  Patient,
  Site,
  Snapshot
}
import de.dnpm.dip.service.DataCounts


object QueryService
{

  trait Ops[F[+_],Env,UseCase <: UseCaseConfig,Err]
  {
    self =>
    
    type PatientRecord = UseCase#PatientRecord
    type Criteria      = UseCase#Criteria
    type Results       = UseCase#Results
   
    def federatedQueriesActive: Boolean

    def sites(
      implicit env: Env
    ): F[Sites]
   
    def !(
      cmd: Query.Command[Criteria]
    )(
      implicit
      env: Env,
      querier: Querier
    ): F[Either[Query.Error,Query[Criteria]]]
    
    
    def get(
      id: Query.Id
    )(
      implicit
      env: Env,
      querier: Querier
    ): F[Option[Query[Criteria]]]
    
    
    // For Admin purposes
    def queries(
      implicit
      env: Env,
      querier: Querier
    ): F[Seq[Query[Criteria]]]
    
   
    def resultSet(
      id: Query.Id
    )(
      implicit
      env: Env,
      querier: Querier,
    ): F[Option[Results]]
    
    
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
    ): F[Either[Err,Option[Snapshot[PatientRecord]]]]
    
    
    // Peer-to-peer Ops
    def !(
      req: FederatedQuery[Criteria,PatientRecord]
    )(
      implicit env: Env
    ): F[Either[Err,req.ResultType]]
    
    
    def !(
      req: PatientRecordRequest[PatientRecord]
    )(
      implicit env: Env
    ): F[Either[Err,req.ResultType]]
    
  }


  sealed trait DataCommand[+T]
  final case class Save[T](record: T) extends DataCommand[T]
  final case class Delete(id: Id[Patient]) extends DataCommand[Nothing]

  sealed trait DataOutcome[+T]
  final case class Saved[T](snp: Snapshot[T]) extends DataOutcome[T]
  final case class Deleted(id: Id[Patient]) extends DataOutcome[Nothing]

  sealed trait DataError
  final case class GenericError(msg: String) extends DataError


  trait DataOps[F[_],Env,T] extends DataCounts.Ops[F,Env]
  {
    def !(cmd: DataCommand[T])(
      implicit env: Env
    ): F[Either[DataError,DataOutcome[T]]]
  }

/*
  final case class StatusInfo
  (
    federatedQueriesActive: Boolean,
    total: Int
  )

  object StatusInfo
  {
    trait Ops[F[_],Env]
    {
      def statusInfo(
        implicit env: Env
      ): F[QueryService.StatusInfo]
    }

    implicit val format: OFormat[StatusInfo] =
      Json.format[StatusInfo]
  }
*/
}


trait QueryService[F[+_],Env,UseCase <: UseCaseConfig]
extends QueryService.Ops[F,Env,UseCase,String]
with QueryService.DataOps[F,Env,UseCase#PatientRecord]
with PreparedQueryOps[F,Env,UseCase#Criteria,String]
