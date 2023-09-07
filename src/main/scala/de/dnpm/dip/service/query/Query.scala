package de.dnpm.dip.service.query


import java.time.{
  Instant,
  LocalDateTime
}
import cats.Applicative
import de.dnpm.dip.coding.{
  Coding,
//  CodedEnum,
//  DefaultCodeSystem
  CodeSystem,
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
  implicit val format = Json.valueFormat[Querier]
}


final case class Query[
  Criteria,
  Filters <: Query.Filters
](
  id: Query.Id,
  submittedAt: LocalDateTime,
  querier: Querier,
  mode: Coding[Query.Mode],
//  mode: Query.Mode.Value,
  criteria: Criteria,
  filters: Filters,
  expiresAfter: Int,
  lastUpdate: Instant
)


object Query
{

  case class Id(value: String) extends AnyVal

/*  
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

    object Provider extends SingleCodeSystemProvider(codeSystem)

    final class ProviderSPI extends CodeSystemProviderSPI
    {
      override def getInstance[F[_]]: CodeSystemProvider[Any,F,Applicative[F]] =
        new Provider.Facade[F]
    }

  }
*/

  sealed trait Mode
  object Mode
  {
    val Local     = "local"
    val Federated = "federated"

    implicit val system =
      Coding.System[Mode]("dnpm-dip/query/mode")

    implicit val codeSystem =
      CodeSystem[Mode](
        uri     = Coding.System[Mode].uri,
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


  trait Filters
  {
    val patientFilter: PatientFilter
  }


  sealed abstract class Command[+Criteria,+Fltrs <: Filters]

  final case class Submit[Criteria]
  ( 
//    mode: Query.Mode.Value,
    mode: Coding[Query.Mode],
    criteria: Criteria
  )
  extends Command[Criteria,Nothing]

  final case class Update[Criteria]
  ( 
    id: Id,
//    mode: Option[Query.Mode.Value],
    mode: Option[Coding[Query.Mode]],
    criteria: Option[Criteria]
  )
  extends Command[Criteria,Nothing]

  final case class ApplyFilters[Fltrs <: Filters]
  (
    id: Id,
    filters: Fltrs
  )
  extends Command[Nothing,Fltrs]


  implicit val formatQueryId           = Json.valueFormat[Id]
//  implicit val formatMode              = Json.formatEnum(Mode)

  implicit def formatQuery[Criteria: Format, Fltrs <: Filters: Format] = Json.format[Query[Criteria,Fltrs]]
  implicit def formatSubmit[Criteria: Format]                          = Json.format[Submit[Criteria]]
  implicit def formatUpdate[Criteria: Format]                          = Json.format[Update[Criteria]]
  implicit def formatApplyFilters[Fltrs <: Filters: Format]          = Json.format[ApplyFilters[Fltrs]]

}
