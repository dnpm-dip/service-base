package de.dnpm.dip.service.query


import java.time.LocalDate.{now => today}
import java.time.temporal.ChronoUnit.YEARS 
import de.dnpm.dip.coding.Coding
import de.dnpm.dip.model.{
  ClosedInterval,
  Gender,
  Patient,
  VitalStatus
}
import play.api.libs.json.{
  Json,
  Format
}


final case class PatientFilter
(
  genders: Set[Coding[Gender.Value]],
  ageRange: ClosedInterval[Long],
  vitalStatus: Set[Coding[VitalStatus.Value]]
)


object PatientFilter
{

  import scala.language.implicitConversions

  implicit def patientFilerToPredicate(
    filter: PatientFilter
  ): Patient => Boolean = {
    patient =>

    import VitalStatus._

    filter.genders.exists(_.code == patient.gender.code) &&
    filter.ageRange.contains(patient.age) &&
    filter.vitalStatus.exists(_.code == patient.vitalStatus.code)
      
  }

  implicit val format: Format[PatientFilter] =
    Json.format[PatientFilter]

}
