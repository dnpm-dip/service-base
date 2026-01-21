package de.dnpm.dip.service.validation


import cats.syntax.validated._
import de.dnpm.dip.model.PatientRecord


final class FakeValidationService[F[+_],T <: PatientRecord] extends BaseValidationService[F,T](
  _.validNel,
  new InMemRepository[F,T]
)

/*
final class FakeValidationService[F[+_]] extends BaseValidationService[F,DummyPatientRecord](
  _.validNel,
  new InMemRepository[F,DummyPatientRecord]
)
*/
