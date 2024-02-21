package de.dnpm.dip.service.query



import de.dnpm.dip.service.auth.{
  Permission,
  Permissions
}



abstract class QueryPermissions
(
  useCase: String
)
extends Enumeration 
with Permissions
{

  import scala.language.implicitConversions

  val SubmitQuery        = Value(s"${useCase}SubmitQuery")
  val ReadResultSummary  = Value(s"${useCase}ReadResultSummary")
  val ReadPatientMatches = Value(s"${useCase}ReadPatientMatches")
  val ReadPatientRecord  = Value(s"${useCase}ReadPatientRecord")


  implicit def toPermission(v: Value): Permission =
    Permission(v.toString)


  override lazy val permissions: Set[Permission] =
    this.values
      .map(_.toString)
      .toSet
      .map(Permission(_))

  def unapply(p: String): Option[Value] =
    this.values.find(_.toString == p)

}


/*
abstract class QueryPermissions
(
  useCase: String
)
extends Permissions
{

  private val prefix =
    useCase.toLowerCase

  val SubmitQuery =
    Permission(s"${prefix}_query_submit")

  val ReadResultSummary =
    Permission(s"${prefix}_result_summary_read")

  val ReadPatientMatches =
    Permission(s"${prefix}_patient_matches_read")

  val ReadPatientRecord =
    Permission(s"${prefix}_patient_record_read")


  override def permissions: Set[Permission] =
    Set(
      SubmitQuery,
      ReadResultSummary,
      ReadPatientMatches,
      ReadPatientRecord
    )


}
*/
