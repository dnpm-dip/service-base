package de.dnpm.dip.service


import java.time.LocalDate
import cats.data.NonEmptyList
import de.dnpm.dip.util.Completer
import de.dnpm.dip.coding.Coding
import de.dnpm.dip.model.{
  CarePlan,
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


final case class DummyCarePlan
(
  id: Id[CarePlan],
  patient: Reference[Patient],
  issuedOn: LocalDate,
  noSequencingPerformedReason: Option[Coding[CarePlan.NoSequencingPerformedReason.Value]]
)
extends CarePlan
{
  val reason = None
  val therapyRecommendations = None
  val medicationRecommendations = None
  val studyEnrollmentRecommendations = None
  val notes = None
}


final case class DummyPatientRecord
(
  patient: Patient,
  episodesOfCare: NonEmptyList[DummyEpisodeOfCare],
  diagnoses: NonEmptyList[DummyDiagnosis],
  carePlans: NonEmptyList[DummyCarePlan]
)
extends PatientRecord
{
  override val ngsReports = None
  override val followUps = None
  override val systemicTherapies = None
  override def getCarePlans = carePlans.toList
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

  implicit val formatCarePlan: OFormat[DummyCarePlan] =
    Json.format[DummyCarePlan]

  implicit val format: OFormat[DummyPatientRecord] =
    Json.format[DummyPatientRecord]

}
