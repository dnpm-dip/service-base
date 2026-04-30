package de.dnpm.dip.service.mvh


import cats.Monad
import de.dnpm.dip.model.PatientRecord


final class FakeMVHService[F[+_],T <: PatientRecord](
  useCase: UseCase.Value = UseCase.MTB,
  repository: Repository[F,Monad[F],T] = new InMemRepository[F,T]
)
extends BaseMVHService[F,T](
  useCase,
  repository
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
