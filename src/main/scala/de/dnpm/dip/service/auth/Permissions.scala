package de.dnpm.dip.service.auth


import de.dnpm.dip.util.{
  SPI,
  SPILoader
}



final case class Permission(name: String)

final case class Role
(
  name: String,
  permissions: Set[Permission]
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
