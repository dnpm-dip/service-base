package de.dnpm.dip.service.auth


import de.dnpm.dip.util.{
  SPI,
  SPILoader
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

