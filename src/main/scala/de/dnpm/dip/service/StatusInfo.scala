package de.dnpm.dip.service


import java.time.LocalDateTime
import de.dnpm.dip.coding.Coding
import de.dnpm.dip.model.Site
import de.dnpm.dip.service.mvh.MVHService
import de.dnpm.dip.service.query.QueryService
import de.dnpm.dip.service.validation.ValidationService
import play.api.libs.json.{
  Json,
  Writes
}


final case class StatusInfo
(
  site: Coding[Site],
  datetime: LocalDateTime,
  validation: ValidationService.StatusInfo,
  mvGenomSeq: MVHService.StatusInfo,
  query: QueryService.StatusInfo
)


object StatusInfo
{
  implicit val format: Writes[StatusInfo] =
    Json.writes[StatusInfo]
}

