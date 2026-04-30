package de.dnpm.dip.service


import java.time.{
  LocalDate,
  LocalDateTime
}
import cats.data.NonEmptyList
import de.dnpm.dip.coding.Coding
import de.dnpm.dip.model.{
  Period,
  Site
}
import de.dnpm.dip.service.mvh.MVHService
import de.dnpm.dip.service.query.QueryService
import play.api.libs.json.{
  Json,
  OFormat,
  OWrites
}
import StatusInfo._


/*
sealed trait StatusInfo
{
  def validation: ValidationService.StatusInfo
  def mvGenomSeq: MVHService.StatusInfo
  def query: QueryService.StatusInfo
}
*/

object StatusInfo
{
  final case class Criteria
  (
    episodeOfCarePeriod: Option[Period[LocalDate]] = None
  )


  implicit val formatCriteria: OFormat[Criteria] =
    Json.format[Criteria]
}


final case class LocalStatusInfo
(
  site: Coding[Site],
  datetime: LocalDateTime,
  mvGenomSeq: MVHService.StatusInfo,
  query: QueryService.StatusInfo
)

object LocalStatusInfo
{

  final case class Request
  (
    origin: Coding[Site],
    criteria: StatusInfo.Criteria 
  )
  extends PeerToPeerRequest
  {
    type ResultType = LocalStatusInfo
  }


  implicit val writesRequest: OWrites[Request] =
    Json.writes[Request]

  implicit val format: OFormat[LocalStatusInfo] =
    Json.format[LocalStatusInfo]
}

final case class FederatedStatusInfo
(
  compiledAt: LocalDateTime,
  sites: List[Coding[Site]],
  criteria: StatusInfo.Criteria,
  components: List[LocalStatusInfo],
  errors: Option[NonEmptyList[String]]
)

object FederatedStatusInfo
{ 
  import de.dnpm.dip.util.json.writesNel

  implicit val format: OWrites[FederatedStatusInfo] =
    Json.writes[FederatedStatusInfo]
}
