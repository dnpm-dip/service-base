package de.dnpm.dip.service.mvh


import cats.Monad
import de.dnpm.dip.model.PatientRecord


final class FakeMVHService[F[_],T <: PatientRecord] extends BaseMVHService[F,T](
  UseCase.MTB,
  new InMemRepository[F,T]
){

  type ReportType = BaseReport

  override def sequenceTypes(record: T) = None

  override def report(
    criteria: Report.Criteria
  )(
    implicit env: Monad[F]
  ): F[ReportType] =
    env.map(baseReport(criteria))(_._1)

}

/*
final class FakeMVHService[F[_]] extends BaseMVHService[F,DummyPatientRecord](
  UseCase.MTB,
  new InMemRepository[F,DummyPatientRecord]
){

  type ReportType = BaseReport

  override def sequenceTypes(record: DummyPatientRecord) = None

  override def report(
    criteria: Report.Criteria
  )(
    implicit env: Monad[F]
  ): F[ReportType] =
    env.map(baseReport(criteria))(_._1)

}
*/
