package de.dnpm.dip.service.validation



import de.dnpm.dip.service.auth.{
  Permission,
  Permissions,
  PermissionEnumeration
}



abstract class ValidationPermissions(
  useCase: String
)
extends PermissionEnumeration
{

  val ViewValidationInfos      = Value(s"${useCase}ViewValidationInfos")
  val ViewValidationReport     = Value(s"${useCase}ViewValidationReport")
  val ViewInvalidPatientRecord = Value(s"${useCase}ViewInvalidPatientRecord")

}

