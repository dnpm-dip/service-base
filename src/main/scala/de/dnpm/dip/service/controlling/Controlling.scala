package de.dnpm.dip.service.controlling


import java.time.{
  LocalDate,
  LocalDateTime
}
import cats.Semigroup
import cats.data.NonEmptyList
import de.dnpm.dip.coding.Coding
import de.dnpm.dip.model.{
  EpisodeOfCare,
  Period,
  PatientRecord,
  Site
}
import de.dnpm.dip.service.PeerToPeerRequest
import play.api.libs.json.{
  Json,
  OFormat,
  OWrites
}


final case class PatientDataCounts
( 
  total: Int,
  matchingCriteria: Option[Int]
)

object PatientDataCounts
{

  def from[T <: PatientRecord](
    patientRecords: Iterable[T],
    criteria: Option[Controlling.Criteria]
  ): PatientDataCounts =
    PatientDataCounts(
      patientRecords.size,
      criteria.map {
        case Controlling.Criteria(period) =>
          patientRecords.count(_.episodesOfCare.exists(eoc => period contains eoc.period.start))
      }
    )

  def fromEpisodesOfCare(
    episodesOfCare: Iterable[NonEmptyList[EpisodeOfCare]],
    criteria: Option[Controlling.Criteria]
  ): PatientDataCounts =
    PatientDataCounts(
      episodesOfCare.size,
      criteria.map {
        case Controlling.Criteria(period) =>
          episodesOfCare.count(_.exists(eoc => period contains eoc.period.start))
      }
    )

  implicit val format: OFormat[PatientDataCounts] =
    Json.format[PatientDataCounts]


  import cats.syntax.semigroup._

  implicit val semigroup: Semigroup[PatientDataCounts] =
    Semigroup.instance(
      (l,r) => PatientDataCounts(
        l.total + r.total,
        l.matchingCriteria combine r.matchingCriteria
      )
    )
}


object Controlling
{

  final case class Criteria
  (
    episodeOfCarePeriod: Period[LocalDate]
  )


  trait Ops[F[_],Env]
  { 
    def patientDataCounts(
      criteria: Option[Criteria]
    )(
      implicit env: Env
    ): F[PatientDataCounts]
  }

  implicit val formatCriteria: OFormat[Criteria] =
    Json.format[Criteria]
}

sealed trait ControllingInfo
{
  val mvGenomSeqCounts: PatientDataCounts
  val queryCounts: PatientDataCounts
}

final case class LocalControllingInfo
(
  site: Coding[Site],
  datetime: LocalDateTime,
  mvGenomSeqCounts: PatientDataCounts,
  queryCounts: PatientDataCounts
)
extends ControllingInfo

object LocalControllingInfo
{

  final case class Request
  (
    origin: Coding[Site],
    criteria: Option[Controlling.Criteria]
  )
  extends PeerToPeerRequest
  {
    type ResultType = LocalControllingInfo
  }


  implicit val writesRequest: OWrites[Request] =
    Json.writes[Request]

  implicit val format: OFormat[LocalControllingInfo] =
    Json.format[LocalControllingInfo]
}

final case class FederatedControllingInfo
(
  compiledAt: LocalDateTime,
  sites: List[Coding[Site]],
  criteria: Option[Controlling.Criteria],
  mvGenomSeqCounts: PatientDataCounts,
  queryCounts: PatientDataCounts,
  components: List[LocalControllingInfo],
  errors: Option[NonEmptyList[String]]
)
extends ControllingInfo

object FederatedControllingInfo
{ 
  import de.dnpm.dip.util.json.writesNel

  implicit val format: OWrites[FederatedControllingInfo] =
    Json.writes[FederatedControllingInfo]
}
