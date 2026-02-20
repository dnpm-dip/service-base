package de.dnpm.dip.service.validation


import cats.syntax.validated._
import de.dnpm.dip.model.PatientRecord
import de.dnpm.dip.model.NGSReport.Type._


final class FakeValidationService[F[+_],T <: PatientRecord] extends BaseValidationService[F,T](
  Set(
    GenomeShortRead,
    GenomeLongRead
  ),
  _.validNel,
  new InMemRepository[F,T]
)
