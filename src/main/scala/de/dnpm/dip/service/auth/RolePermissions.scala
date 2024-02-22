package de.dnpm.dip.service.auth



import de.dnpm.dip.util.{
  SPI,
  SPILoader
}



final case class Permission
(
  name: String
)

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



trait Roles
{
  def roles: Set[Role]
}

trait RolesSPI extends SPI[Roles]

object Roles extends SPILoader[RolesSPI]
{
  def getAll: Set[Role] =
    getInstances(this.getClass.getClassLoader)
      .flatMap(_.roles)
      .toSet
}

