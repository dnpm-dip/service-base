package de.dnpm.dip.service.query


import java.time.{
  Instant,
  LocalDateTime
}
import cats.Applicative
import cats.data.NonEmptyList
import play.api.libs.json.{
  Json,
  Reads,
  Writes,
  OWrites,
  Format,
}
import de.dnpm.dip.coding.{
  Coding,
  CodeSystem,
  CodedEnum,
  DefaultCodeSystem,
  CodeSystemProvider,
  CodeSystemProviderSPI,
  SingleCodeSystemProvider
}
import de.dnpm.dip.model.Site
import de.dnpm.dip.service.ConnectionStatus


final case class Querier(value: String) extends AnyVal
object Querier
{
  implicit val format: Format[Querier] =
    Json.valueFormat[Querier]
}



final case class Query[
  Criteria,
  Filter <: Filters[_]
](
  id: Query.Id,
  submittedAt: LocalDateTime,
  querier: Querier,
  mode: Coding[Query.Mode.Value],
  peers: Seq[ConnectionStatus],
  criteria: Criteria,
  filters: Filter,
  expiresAfter: Int,
  lastUpdate: Instant
)


object Query
{

  case class Id(value: String) extends AnyVal

  
  object Mode
  extends CodedEnum("dnpm-dip/query/mode")
  with DefaultCodeSystem
  {
    val Local     = Value("local")
    val Federated = Value("federated")

    override val display = {
      case Local     => "Lokal"
      case Federated => "Föderiert"
    }

    final class ProviderSPI extends CodeSystemProviderSPI
    {
      override def getInstance[F[_]]: CodeSystemProvider[Any,F,Applicative[F]] =
        new Provider.Facade[F]
    }

    implicit val format: Format[Value] =
      Json.formatEnum(this)

  }


  sealed abstract class Command[+Criteria,+Filter <: Filters[_]]

  final case class Submit[Criteria]
  ( 
    mode: Option[Coding[Query.Mode.Value]],
    sites: Option[Set[Coding[Site]]],
    criteria: Criteria
  )
  extends Command[Criteria,Nothing]

  final case class Update[Criteria]
  ( 
    id: Id,
    mode: Option[Coding[Query.Mode.Value]],
    sites: Option[Set[Coding[Site]]],
    criteria: Option[Criteria]
  )
  extends Command[Criteria,Nothing]

  final case class Delete( 
    id: Id,
  )
  extends Command[Nothing,Nothing]


  sealed trait Error
  final case object NoResults extends Error
  final case object InvalidId extends Error
  final case class ConnectionErrors(messages: NonEmptyList[String]) extends Error
  final case class GenericError(message: String) extends Error


  implicit val formatQueryId: Format[Id] =
    Json.valueFormat[Id]

  implicit def formatQuery[
    Criteria: Writes,
    Filter <: Filters[_]: Writes
  ]: OWrites[Query[Criteria,Filter]] =
    Json.writes[Query[Criteria,Filter]]

  implicit def formatSubmit[Criteria: Reads]: Reads[Submit[Criteria]] =
    Json.reads[Submit[Criteria]]

  implicit def formatUpdate[Criteria: Format]: Reads[Update[Criteria]] =
    Json.reads[Update[Criteria]]

  implicit val formatDelete: Reads[Delete] =
    Json.reads[Delete]

}
