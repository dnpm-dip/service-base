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
import play.api.libs.json.{
  Json,
  OFormat,
  OWrites
}


final case class DataCounts
( 
  totalPatients: Int,
  totalEpisodesOfCare: Int,
  episodesOfCareInPeriod: Option[Int]
)

object DataCounts
{

  final case class Criteria
  (
    episodeOfCarePeriod: Period[LocalDate]
  )

  trait Ops[F[_],Env]
  { 
    def dataCounts(
      criteria: Option[Criteria]
    )(
      implicit env: Env
    ): F[DataCounts]
  }

  implicit val formatCriteria: OFormat[Criteria] =
    Json.format[Criteria]

  implicit val format: OFormat[DataCounts] =
    Json.format[DataCounts]
}


final case class LocalDataCounts
(
  site: Coding[Site],
  datetime: LocalDateTime,
  mvGenomSeq: DataCounts,
  query: DataCounts
)

object LocalDataCounts
{

  final case class Request
  (
    origin: Coding[Site],
    criteria: Option[DataCounts.Criteria]
  )
  extends PeerToPeerRequest
  {
    type ResultType = LocalDataCounts
  }


  implicit val writesRequest: OWrites[Request] =
    Json.writes[Request]

  implicit val format: OFormat[LocalDataCounts] =
    Json.format[LocalDataCounts]
}

final case class FederatedDataCounts
(
  compiledAt: LocalDateTime,
  sites: List[Coding[Site]],
  criteria: Option[DataCounts.Criteria],
  components: List[LocalDataCounts],
  errors: Option[NonEmptyList[String]]
)

object FederatedDataCounts
{ 
  import de.dnpm.dip.util.json.writesNel

  implicit val format: OWrites[FederatedDataCounts] =
    Json.writes[FederatedDataCounts]
}
