package de.dnpm.dip.service.auth


import de.dnpm.dip.util.{
  SPI,
  SPILoader
}



final case class Permission
(
  name: String,
  display: String,
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

  val display: Value => String

  val description: Value => String
  

  implicit def toPermission(v: Value): Permission =
    Permission(
      v.toString,
      display(v),
      Some(description(v))
    )


  override lazy val permissions: Set[Permission] =
    this.values
      .toSet[Value]
      .map(toPermission)

  def unapply(p: String): Option[Value] =
    this.values.find(_.toString == p)

}
