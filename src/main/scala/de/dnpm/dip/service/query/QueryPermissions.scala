package de.dnpm.dip.service.query



import de.dnpm.dip.service.auth.{
  Permission,
  Permissions,
  PermissionEnumeration,
  Role,
  Roles
}


abstract class QueryPermissions(val useCase: String) extends PermissionEnumeration
{

  val SubmitQuery        = Value(s"${useCase.toLowerCase}_query_submit")
  val ReadResultSummary  = Value(s"${useCase.toLowerCase}_result_summary_read")
  val ReadPatientMatches = Value(s"${useCase.toLowerCase}_patient_matches_read")
  val ReadPatientRecord  = Value(s"${useCase.toLowerCase}_patient_record_read")


  override val display =
    Map(
      SubmitQuery        -> s"$useCase-Anfragen absetzen",
      ReadResultSummary  -> s"$useCase-Ergebnis-Übersicht einsehen",
      ReadPatientMatches -> s"$useCase-Patienten-Liste einsehen",
      ReadPatientRecord  -> s"$useCase-Patienten-Akte einsehen"
    )


  override val description =
    Map(
      SubmitQuery        -> s"$useCase-Such-Modul: Anfragen absetzen (lokal/föderiert)",
      ReadResultSummary  -> s"$useCase-Such-Modul: Ergebnis-Übersicht abrufen/einsehen",
      ReadPatientMatches -> s"$useCase-Such-Modul: Patienten-Treffer-Liste (Kohorte) abrufen/einsehen",
      ReadPatientRecord  -> s"$useCase-Such-Modul: Patienten-Akte aus Treffer-Liste abrufen/einsehen"
    )

}


abstract class QueryRoles(qp: QueryPermissions) extends Roles
{

  private val useCase = qp.useCase

  import qp._

  val BasicQueryRights =
    Role(
      s"${useCase.toLowerCase}_basic_query_rights",
      s"$useCase: Basis-Rechte Such-Modul",
      (qp.permissions - qp.ReadPatientRecord),
      Some(s"$useCase: Basis-Such-Rechte, d.h. nur Ergebnis-Zusammenfassungen, OHNE Einsicht individueller Patienten-Akten")
    )

  val PrivilegedQueryRights =
    Role(
      s"${useCase.toLowerCase}_privileged_query_rights",
      s"$useCase: Privilegierte Rechte Such-Modul",
      qp.permissions,
      Some(s"$useCase: Privilegierte Such-Rechte, d.h. Einsicht in Ergebnis-Zusammenfassungen UND in individuelle Patienten-Akten")
    )


  override val roles: Set[Role] =
    Set(
      BasicQueryRights,
      PrivilegedQueryRights
    )

}

