package de.dnpm.dip.service.query



import de.dnpm.dip.service.auth.{
  Permission,
  Permissions,
  PermissionEnumeration
}



abstract class QueryPermissions
(
  useCase: String
)
extends PermissionEnumeration
{

  val SubmitQuery        = Value(s"${useCase}SubmitQuery")
  val ReadResultSummary  = Value(s"${useCase}ReadResultSummary")
  val ReadPatientMatches = Value(s"${useCase}ReadPatientMatches")
  val ReadPatientRecord  = Value(s"${useCase}ReadPatientRecord")

}
