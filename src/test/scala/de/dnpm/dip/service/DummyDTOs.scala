package de.dnpm.dip.service


import java.time.LocalDate
import cats.data.NonEmptyList
import de.dnpm.dip.util.Completer
import de.dnpm.dip.model.{
  Diagnosis,
  EpisodeOfCare,
  Id,
  Patient,
  PatientRecord,
  Period,
  Reference
}
import play.api.libs.json.{
  Json,
  OFormat
}

final case class DummyEpisodeOfCare
(
  id: Id[EpisodeOfCare],
  patient: Reference[Patient],
  period: Period[LocalDate],
  diagnoses: Option[List[Reference[Diagnosis]]] = None
)
extends EpisodeOfCare

final case class DummyDiagnosis
(
  id: Id[Diagnosis],
  patient: Reference[Patient],
  recordedOn: LocalDate
)
extends Diagnosis
{
  val notes = None
}


final case class DummyPatientRecord
(
  patient: Patient,
  episodesOfCare: NonEmptyList[DummyEpisodeOfCare],
  diagnoses: NonEmptyList[DummyDiagnosis]
)
extends PatientRecord
{
  override val ngsReports = None

  override val followUps = None

  override val systemicTherapies = None

  override def getCarePlans = List.empty
}


object DummyPatientRecord
{

  import de.dnpm.dip.util.json.{
    readsNel,
    writesNel
  }

  implicit val completer: Completer[DummyPatientRecord] =
    identity[DummyPatientRecord]


  implicit val formatEoC: OFormat[DummyEpisodeOfCare] =
    Json.format[DummyEpisodeOfCare]

  implicit val formatDiagnosis: OFormat[DummyDiagnosis] =
    Json.format[DummyDiagnosis]

  implicit val format: OFormat[DummyPatientRecord] =
    Json.format[DummyPatientRecord]

}
