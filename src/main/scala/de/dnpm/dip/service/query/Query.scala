package de.dnpm.dip.service.query


import java.time.{
  Instant,
  LocalDateTime
}
import cats.Applicative
import de.dnpm.dip.coding.{
  Coding,
  CodeSystem,
  CodedEnum,
  DefaultCodeSystem,
  CodeSystemProvider,
  CodeSystemProviderSPI,
  SingleCodeSystemProvider
}
import play.api.libs.json.{
  Json,
  Format
}


final case class Querier(value: String) extends AnyVal
object Querier
{
  implicit val format: Format[Querier] =
    Json.valueFormat[Querier]
}


final case class Query[
  Criteria,
  Filters <: Query.Filters
](
  id: Query.Id,
  submittedAt: LocalDateTime,
  querier: Querier,
  mode: Coding[Query.Mode.Value],
  criteria: Criteria,
  filters: Filters,
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


/*
  sealed trait Mode
  object Mode
  {
    val Local     = "local"
    val Federated = "federated"

    implicit val system: Coding.System[Mode] =
      Coding.System[Mode]("dnpm-dip/query/mode")

    implicit val codeSystem: CodeSystem[Mode] =
      CodeSystem[Mode](
        name    = "query-mode",
        title   = Some("Query Mode"),
        version = None,
        Local     -> "Lokal",
        Federated -> "Föderiert"
      )

    object Provider extends SingleCodeSystemProvider(codeSystem)

    final class ProviderSPI extends CodeSystemProviderSPI
    {
      override def getInstance[F[_]]: CodeSystemProvider[Any,F,Applicative[F]] =
        new Provider.Facade[F]
    }

  }
*/

  trait Filters
  {
    val patientFilter: PatientFilter
  }


  sealed abstract class Command[+Criteria,+Fltrs <: Filters]

  final case class Submit[Criteria]
  ( 
    mode: Coding[Query.Mode.Value],
    criteria: Criteria
  )
  extends Command[Criteria,Nothing]

  final case class Update[Criteria]
  ( 
    id: Id,
    mode: Option[Coding[Query.Mode.Value]],
    criteria: Option[Criteria]
  )
  extends Command[Criteria,Nothing]

  final case class ApplyFilters[Fltrs <: Filters]
  (
    id: Id,
    filters: Fltrs
  )
  extends Command[Nothing,Fltrs]


  implicit val formatQueryId: Format[Id] =
    Json.valueFormat[Id]

  implicit def formatQuery[
    Criteria: Format,
    Fltrs <: Filters: Format
  ]: Format[Query[Criteria,Fltrs]] =
    Json.format[Query[Criteria,Fltrs]]

  implicit def formatSubmit[Criteria: Format]: Format[Submit[Criteria]] =
    Json.format[Submit[Criteria]]

  implicit def formatUpdate[Criteria: Format]: Format[Update[Criteria]] =
    Json.format[Update[Criteria]]

  implicit def formatApplyFilters[Fltrs <: Filters: Format]: Format[ApplyFilters[Fltrs]] =
    Json.format[ApplyFilters[Fltrs]]

}
