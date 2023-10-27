package de.dnpm.dip.service.query


import de.dnpm.dip.model.{
  Id,
  Site,
  Age,
  Patient,
  Gender,
  VitalStatus
}
import de.dnpm.dip.coding.Coding
import play.api.libs.json.{
  Json,
  Format
}


final case class PatientMatch[Criteria]
(
  id: Id[Patient],
  managingSite: Option[Coding[Site]],
  gender: Coding[Gender.Value],
  age: Age,
//  age: Long,
  vitalStatus: Coding[VitalStatus.Value],
  matchingCriteria: Criteria
)



object PatientMatch
{

  def of[Criteria](
    patient: Patient,
    matchingCriteria: Criteria
  ): PatientMatch[Criteria] =
    PatientMatch(
      patient.id,
      patient.managingSite,
      patient.gender,
      patient.age,
      patient.vitalStatus,
      matchingCriteria
    )


  implicit def format[Criteria: Format]: Format[PatientMatch[Criteria]] =
    Json.format[PatientMatch[Criteria]]
}

