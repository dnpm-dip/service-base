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
  Format,
  OFormat
}


final case class PatientMatch[Criteria]
(
  id: Id[Patient],
  managingSite: Option[Coding[Site]],
  gender: Coding[Gender.Value],
  age: Age,
  vitalStatus: Coding[VitalStatus.Value],
  matchingCriteria: Option[Criteria]
)



object PatientMatch
{

  def of[Criteria](
    patient: Patient,
    matchingCriteria: Option[Criteria]
  ): PatientMatch[Criteria] =
    PatientMatch(
      patient.id,
      patient.managingSite, // Always defined: site is set upon entry of a dataset into the DNPM system 'context' (i.e. upon import)
      patient.gender,
      patient.age,
      patient.vitalStatus,
      matchingCriteria
    )


  implicit def format[Criteria: Format]: OFormat[PatientMatch[Criteria]] =
    Json.format[PatientMatch[Criteria]]
}

