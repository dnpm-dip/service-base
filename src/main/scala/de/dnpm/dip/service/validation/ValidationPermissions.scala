package de.dnpm.dip.service.validation



import de.dnpm.dip.service.auth.{
  Permission,
  Permissions,
  PermissionEnumeration,
  Role,
  Roles
}



abstract class ValidationPermissions(val useCase: String) extends PermissionEnumeration
{

  val ReadValidationInfos      = Value(s"${useCase.toLowerCase}_validation_infos_read")
  val ReadValidationReport     = Value(s"${useCase.toLowerCase}_validation_report_read")
  val ReadInvalidPatientRecord = Value(s"${useCase.toLowerCase}_validation_patient_record_read")

  override val display =
    Map(
      ReadValidationInfos      -> s"$useCase Validierungsübersicht einsehen",
      ReadValidationReport     -> s"$useCase Validierungsbericht einsehen",
      ReadInvalidPatientRecord -> s"$useCase Invalide Patienten-Akte einsehen"
    )

  override val description =
    Map(
      ReadValidationInfos      -> s"$useCase-Daten-Validierung: Liste von Datensätzen mit Daten-Qualitätsproblemen abrufen/einsehen",
      ReadValidationReport     -> s"$useCase-Daten-Validierung: Validierungsbericht eines gebenen Datensatzes (Patienten) abrufen/einsehen",
      ReadInvalidPatientRecord -> s"$useCase-Daten-Validierung: Datensatz (Patienten-Akte) zu einem gegebenen Validierungsbericht abrufen/einsehen"
    )

}


abstract class ValidationRoles(basePermissions: ValidationPermissions) extends Roles
{

  private val useCase = basePermissions.useCase

  val Documentarist =
    Role(
      s"${useCase.toLowerCase}_data_qc_rights",
      s"${useCase}: Zugriffsrechte Daten-Prüfmodul",
      basePermissions.permissions,
      Some(s"Zugriffsrechte auf $useCase-Daten-Prüfmodul zur Behebung von Daten-Qualitätsproblemen")
    )

  override val roles: Set[Role] =
    Set(Documentarist)

}


/*
abstract class ValidationRoles(basePermissions: ValidationPermissions) extends Roles
{

  private val useCase = basePermissions.useCase

  val Documentarist =
    Role(
      s"${useCase.toLowerCase}_documentarist",
      s"${useCase}-Documentar:in",
      basePermissions.permissions,
      Some(s"$useCase: Dokumentar:in zur Behebung von Daten-Qualitätsproblemen")
    )

  override val roles: Set[Role] =
    Set(Documentarist)

}
*/
