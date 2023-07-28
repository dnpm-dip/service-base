package de.dnpm.dip.service.query


import java.time.{
  Instant,
  LocalDateTime
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
  Parameters,
  Filters <: Query.Filters
](
  id: Query.Id,
  submittedAt: LocalDateTime,
  querier: Querier,
  mode: Query.Mode.Value,
  parameters: Parameters,
  filters: Filters,
  lastUpdate: Instant
)


object Query
{

  case class Id(value: String) extends AnyVal

  object Mode extends Enumeration
  {
    val Local, Federated = Value
  }


  trait Filters
  {
    val patientFilter: PatientFilter
  }


  sealed abstract class Command[+Parameters,+Fltrs <: Filters]

  final case class Submit[Parameters]
  ( 
    mode: Query.Mode.Value,
    parameters: Parameters
  )
  extends Command[Parameters,Nothing]

  final case class Update[Parameters]
  ( 
    id: Id,
    mode: Option[Query.Mode.Value],
    parameters: Option[Parameters]
  )
  extends Command[Parameters,Nothing]

  final case class ApplyFilters[Fltrs <: Filters]
  (
    id: Id,
    filters: Fltrs
  )
  extends Command[Nothing,Fltrs]


  implicit val formatQueryId           = Json.valueFormat[Id]
  implicit val formatMode              = Json.formatEnum(Mode)

  implicit def formatQuery[Params: Format, Fltrs <: Filters: Format] = Json.format[Query[Params,Fltrs]]
  implicit def formatSubmit[Params: Format]                          = Json.format[Submit[Params]]
  implicit def formatUpdate[Params: Format]                          = Json.format[Update[Params]]
  implicit def formatApplyFilters[Fltrs <: Filters: Format]          = Json.format[ApplyFilters[Fltrs]]

}
