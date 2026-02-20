package de.dnpm.dip.service.validation


import cats.syntax.validated._
import de.dnpm.dip.model.PatientRecord
import de.dnpm.dip.model.NGSReport
import NGSReport.Type.{
  GenomeShortRead,
  GenomeLongRead
}


final class FakeValidationService[F[+_],T <: PatientRecord](
  admissibleSequencingTypes: Set[NGSReport.Type.Value] = Set(GenomeShortRead,GenomeLongRead),
)
extends BaseValidationService[F,T](
  admissibleSequencingTypes,
  _.validNel,
  new InMemRepository[F,T]
)
