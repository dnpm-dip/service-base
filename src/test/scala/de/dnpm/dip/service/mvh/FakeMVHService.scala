package de.dnpm.dip.service.mvh


import cats.Monad
import de.dnpm.dip.model.PatientRecord


final class FakeMVHService[F[+_],T <: PatientRecord] extends BaseMVHService[F,T](
  UseCase.MTB,
  new InMemRepository[F,T]
){

  type ReportType = BaseReport

  override def diagnosticExtent(record: T) = None

  override def sequenceTypes(record: T) = None

  override def report(
    criteria: Report.Criteria
  )(
    implicit env: Monad[F]
  ): F[ReportType] =
    env.map(baseReport(criteria))(_._1)

}
