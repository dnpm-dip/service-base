package de.dnpm.dip.service.validation


import de.dnpm.dip.model.{
  Id,
  Patient
}
import play.api.libs.json.{
  Json,
  OWrites
}


final case class ValidationInfo
(
  id: Id[Patient],
  issues: Map[String,Int]
)


object ValidationInfo
{
  implicit val format: OWrites[ValidationInfo] =
    Json.writes[ValidationInfo]
}

