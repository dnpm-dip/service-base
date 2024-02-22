package de.dnpm.dip.service.query



import de.dnpm.dip.service.auth.{
  Permission,
  Permissions
}



abstract class QueryPermissions
(
  useCase: String
)
extends Enumeration with Permissions
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

