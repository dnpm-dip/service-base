package de.dnpm.dip.service


import de.dnpm.dip.service.mvh.MVHService
import de.dnpm.dip.service.query.QueryService
import de.dnpm.dip.service.validation.ValidationService
import play.api.libs.json.{
  Json,
  Writes
}


final case class StatusInfo
(
  validationService: ValidationService.StatusInfo,
  mvhService: MVHService.StatusInfo,
  queryService: QueryService.StatusInfo
)


object StatusInfo
{
  implicit val format: Writes[StatusInfo] =
    Json.writes[StatusInfo]
}

