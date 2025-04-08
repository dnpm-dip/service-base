package de.dnpm.dip.service.mvh


import play.api.libs.json.{
  Json,
  Format,
}


object UseCase extends Enumeration
{
  val MTB,RD = Value

  implicit val format: Format[Value] =
    Json.formatEnum(this)
}

