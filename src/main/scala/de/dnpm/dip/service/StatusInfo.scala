package de.dnpm.dip.service


import java.time.{
  LocalDate,
  LocalDateTime
}
import de.dnpm.dip.coding.Coding
import de.dnpm.dip.model.{
  Period,
  Site
}
import de.dnpm.dip.service.mvh.MVHService
import de.dnpm.dip.service.query.QueryService
import de.dnpm.dip.service.validation.ValidationService
import play.api.libs.json.{
  Json,
  OWrites
}


sealed trait StatusInfo
{
  def validation: ValidationService.StatusInfo
  def mvGenomSeq: MVHService.StatusInfo
  def query: QueryService.StatusInfo
}

final case class LocalStatusInfo
(
  site: Coding[Site],
  datetime: LocalDateTime,
  validation: ValidationService.StatusInfo,
  mvGenomSeq: MVHService.StatusInfo,
  query: QueryService.StatusInfo
)
extends StatusInfo


object LocalStatusInfo
{
  implicit val format: OWrites[LocalStatusInfo] =
    Json.writes[LocalStatusInfo]
}

final case class AggregatedStatusInfo
(
  datetime: LocalDateTime,
  sites: List[Coding[Site]],
  criteria: StatusInfo.Criteria,
  validation: ValidationService.StatusInfo,
  mvGenomSeq: MVHService.StatusInfo,
  query: QueryService.StatusInfo,
  components: List[LocalStatusInfo]
)
extends StatusInfo


object StatusInfo
{

  final case class Criteria
  (
    episodeOfCarePeriod: Option[Period[LocalDate]]
  )

  final case class Request
  (
    sites: Option[Set[Coding[Site]]],
    criteria: Criteria 
  )

//  implicit val format: OWrites[StatusInfo] =
//    Json.writes[StatusInfo]
}

