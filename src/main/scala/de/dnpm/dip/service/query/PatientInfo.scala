package de.dnpm.dip.service.query


import play.api.libs.json.Json
import de.dnpm.dip.model.{
  Id,
  Site,
  Patient,
  Gender,
  VitalStatus
}
import de.dnpm.dip.coding.Coding
import de.dnpm.dip.coding.icd.ICD10GM


final case class PatientInfo
(
  id: Id[Patient],
  managingSite: Coding[Site],
  gender: Coding[Gender.Value],
  age: Int,
  diagnoses: List[Coding[ICD10GM]],
  vitalStatus: Coding[VitalStatus.Value]
)

object PatientInfo
{
  implicit val format = Json.format[PatientInfo]
}
