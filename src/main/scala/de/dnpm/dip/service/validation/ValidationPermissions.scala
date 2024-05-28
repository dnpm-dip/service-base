package de.dnpm.dip.service.validation



import de.dnpm.dip.service.auth.{
  Permission,
  Permissions,
  PermissionEnumeration,
  Role,
  Roles
}



abstract class ValidationPermissions(
  useCase: String
)
extends PermissionEnumeration
{

  val ReadValidationInfos      = Value(s"${useCase}_validation_infos_read")
  val ReadValidationReport     = Value(s"${useCase}_validation_report_read")
  val ReadInvalidPatientRecord = Value(s"${useCase}_validation_patient_record_read")

//  val ReadValidationInfos      = Value(s"${useCase}ReadValidationInfos")
//  val ReadValidationReport     = Value(s"${useCase}ReadValidationReport")
//  val ReadInvalidPatientRecord = Value(s"${useCase}ReadInvalidPatientRecord")

  override val descriptions =
    Map(
      ReadValidationInfos      -> s"$useCase-Daten-Validierungsmodul: Liste von Datensätzen mit Daten-Qualitätsproblemen abrufen/einsehen",
      ReadValidationReport     -> s"$useCase-Daten-Validierungsmodul: Validierungsbericht eines gebenen Datensatzes (Patienten) abrufen/einsehen",
      ReadInvalidPatientRecord -> s"$useCase-Daten-Validierungsmodul: Datensatz (Patienten-Akte) zu einem gegebenen Validierungsbericht abrufen/einsehen"
    )

}


abstract class ValidationRoles(
  useCase: String,
  basePermissions: ValidationPermissions
)
extends Roles
{

  val Documentarist =
    Role(
      s"$useCase-Documentarist",
      basePermissions.permissions,
      Some(s"$useCase: Dokumentar:in zur Behebung von Daten-Qualitätsproblemen")
    )

  override val roles: Set[Role] =
    Set(Documentarist)

}

