package de.dnpm.dip.service


import java.time.LocalDate
import cats.data.NonEmptyList
import de.dnpm.dip.util.Completer
import de.dnpm.dip.coding.Coding
import de.dnpm.dip.model.{
  CarePlan,
  Diagnosis,
  EpisodeOfCare,
  FollowUp,
  Id,
  NGSReport,
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
  override val notes = None
}

final case class DummyNGSReport
(
  id: Id[DummyNGSReport],
  patient: Reference[Patient],
  issuedOn: LocalDate,
  `type`: Coding[NGSReport.Type.Value]
)
extends NGSReport
{
  override val notes = None
  override def variants = Seq.empty
}


final case class DummyCarePlan
(
  id: Id[CarePlan],
  patient: Reference[Patient],
  issuedOn: LocalDate,
  boardType: Option[Coding[CarePlan.BoardType.Value]],
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
  carePlans: NonEmptyList[DummyCarePlan],
  ngsReports: Option[List[DummyNGSReport]],
  followUps: Option[List[FollowUp]]
)
extends PatientRecord
{
  override def getCarePlans = carePlans.toList
  override val systemicTherapies = None
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

  implicit val formatNGSReport: OFormat[DummyNGSReport] =
    Json.format[DummyNGSReport]

  implicit val format: OFormat[DummyPatientRecord] =
    Json.format[DummyPatientRecord]

}
