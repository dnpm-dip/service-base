package de.dnpm.dip.service.auth


import de.dnpm.dip.util.{
  SPI,
  SPILoader
}



final case class Permission
(
  name: String,
  description: Option[String] = None
)


trait Permissions
{
  def permissions: Set[Permission]
}

trait PermissionsSPI extends SPI[Permissions]

object Permissions extends SPILoader[PermissionsSPI]
{
  def getAll: Set[Permission] =
    getInstances(this.getClass.getClassLoader)
      .flatMap(_.permissions)
      .toSet
}


trait PermissionEnumeration extends Enumeration with Permissions
{

  import scala.language.implicitConversions

  val descriptions: Value => String
  

  implicit def toPermission(v: Value): Permission =
    Permission(v.toString)

  override lazy val permissions: Set[Permission] =
    this.values
      .toSet[Value]
      .map(v => v.toString -> descriptions(v))
      .map { case (name,desc) => Permission(name,Some(desc)) }

  def unapply(p: String): Option[Value] =
    this.values.find(_.toString == p)

}
