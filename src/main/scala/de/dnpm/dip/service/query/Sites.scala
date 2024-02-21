package de.dnpm.dip.service.query


import de.dnpm.dip.coding.Coding
import de.dnpm.dip.model.Site
import play.api.libs.json.{
  Json,
  OWrites
}


final case class Sites
(
  local: Coding[Site],
  others: List[Coding[Site]]
)


object Sites
{
  implicit val format: OWrites[Sites] =
    Json.writes[Sites]
}
