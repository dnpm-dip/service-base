package de.dnpm.dip.service.auth


import de.dnpm.dip.util.{
  SPI,
  SPILoader
}


final case class Role
(
  name: String,
  display: String,
  permissions: Set[Permission],
  description: Option[String] = None
)


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

