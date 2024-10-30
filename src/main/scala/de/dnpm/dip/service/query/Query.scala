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
  Format
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
import de.dnpm.dip.model.{
  Site,
  Snapshot
}
import de.dnpm.dip.service.ConnectionStatus


final case class Querier(value: String) extends AnyVal
object Querier
{
  implicit val format: Format[Querier] =
    Json.valueFormat[Querier]
}


final case class Query[Criteria]
(
  id: Query.Id,
  submittedAt: LocalDateTime,
  querier: Querier,
  mode: Coding[Query.Mode.Value],
  peers: Seq[ConnectionStatus],
  criteria: Option[Criteria],
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
    val Custom    = Value("custom")

    override val display =
      Map(
        Local     -> "Lokal",
        Federated -> "Föderiert (Standort-übergreifend)",
        Custom    -> "Nutzer-definiert (gezielte Standort-Auswahl)"
      )

    final class ProviderSPI extends CodeSystemProviderSPI
    {
      override def getInstance[F[_]]: CodeSystemProvider[Any,F,Applicative[F]] =
        new Provider.Facade[F]
    }

    implicit val format: Format[Value] =
      Json.formatEnum(this)

  }


  sealed abstract class Command[+Criteria]

  final case class Submit[Criteria]
  ( 
    mode: Coding[Query.Mode.Value],
    sites: Option[Set[Coding[Site]]],
    criteria: Option[Criteria]
  )
  extends Command[Criteria]

  final case class Update[Criteria]
  ( 
    id: Id,
    mode: Option[Coding[Query.Mode.Value]],
    sites: Option[Set[Coding[Site]]],
    criteria: Option[Criteria]
  )
  extends Command[Criteria]

  final case class Delete( 
    id: Id,
  )
  extends Command[Nothing]



  final case class Match[PatientRecord,Criteria]
  (
    record: Snapshot[PatientRecord],
    matchingCriteria: Option[Criteria]
  ) 
  object Match
  {
    implicit def reads[PatientRecord: Reads,Criteria: Reads]: Reads[Match[PatientRecord,Criteria]] =
      Json.reads[Match[PatientRecord,Criteria]]
    
    implicit def writes[PatientRecord: Writes,Criteria: Writes]: Writes[Match[PatientRecord,Criteria]] =
      Json.writes[Match[PatientRecord,Criteria]]
  }



  sealed trait Error
  final case object NoResults extends Error
  final case object InvalidId extends Error
  final case class ConnectionErrors(messages: NonEmptyList[String]) extends Error
  final case class GenericError(message: String) extends Error


  implicit val formatQueryId: Format[Id] =
    Json.valueFormat[Id]

  implicit def formatQuery[Criteria: Writes]: OWrites[Query[Criteria]] =
    Json.writes[Query[Criteria]]

  implicit def formatSubmit[Criteria: Format]: Format[Submit[Criteria]] =
    Json.format[Submit[Criteria]]

  implicit def formatUpdate[Criteria: Format]: Format[Update[Criteria]] =
    Json.format[Update[Criteria]]

  implicit val formatDelete: Reads[Delete] =
    Json.reads[Delete]

}
