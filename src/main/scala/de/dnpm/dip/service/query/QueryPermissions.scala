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

  val SubmitQuery        = Value(s"${useCase.toLowerCase}_query_submit")
  val ReadResultSummary  = Value(s"${useCase.toLowerCase}_result_summary_read")
  val ReadPatientMatches = Value(s"${useCase.toLowerCase}_patient_matches_read")
  val ReadPatientRecord  = Value(s"${useCase.toLowerCase}_patient_record_read")


  override val descriptions =
    Map(
      SubmitQuery        -> s"$useCase-Such-Modul: Anfragen absetzen (lokal/föderiert)",
      ReadResultSummary  -> s"$useCase-Such-Modul: Ergebnis-Übersicht abrufen/einsehen",
      ReadPatientMatches -> s"$useCase-Such-Modul: Patienten-Treffer-Liste (Kohorte) abrufen/einsehen",
      ReadPatientRecord  -> s"$useCase-Such-Modul: Patienten-Akte aus Treffer-Liste abrufen/einsehen"
    )

}
