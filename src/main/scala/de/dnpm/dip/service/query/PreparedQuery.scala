package de.dnpm.dip.service.query


import java.time.{
  Instant,
  LocalDateTime
}
import cats.data.EitherNel
import play.api.libs.json.{
  Json,
  Format,
  OFormat
}


final case class PreparedQuery[Criteria]
(
  id: PreparedQuery.Id,
  querier: Querier,
  name: String,
  criteria: Criteria,
  createdAt: LocalDateTime,
  lastUpdate: Instant
)


object PreparedQuery
{

  final case class Id(value: String) extends AnyVal


  sealed abstract class Command[+Criteria]

  final case class Create[Criteria]
  (
    name: String,
    criteria: Criteria
  )
  extends Command[Criteria]

  final case class Update[Criteria]
  (
    id: PreparedQuery.Id,
    name: Option[String],
    criteria: Option[Criteria]
  )
  extends Command[Criteria]

  final case class Delete
  (
    id: PreparedQuery.Id,
  )
  extends Command[Nothing]


  final case class Query(
    querier: Option[Querier] = None
  )


  implicit def formatId: Format[Id] =
    Json.valueFormat[Id]

  implicit def formatPrepQuery[Criteria: Format]: OFormat[PreparedQuery[Criteria]] =
    Json.format[PreparedQuery[Criteria]]

  implicit def formatCreate[Criteria: Format]: OFormat[Create[Criteria]] =
    Json.format[Create[Criteria]]

  implicit def formatUpdate[Criteria: Format]: OFormat[Update[Criteria]] =
    Json.format[Update[Criteria]]

  implicit val formatDelete: OFormat[Delete] =
    Json.format[Delete]

}


trait PreparedQueryOps[
  F[+_],
  Env,
  Criteria,
  Error
]
{

  def !(
    cmd: PreparedQuery.Command[Criteria]
  )(
    implicit
    env: Env,
    querier: Querier
  ): F[Error EitherNel PreparedQuery[Criteria]] 


  def ?(
    id: PreparedQuery.Id
  )(
    implicit
    env: Env,
    querier: Querier
  ): F[Option[PreparedQuery[Criteria]]] 


  def ?(
    query: PreparedQuery.Query
  )(
    implicit
    env: Env,
    querier: Querier
  ): F[Seq[PreparedQuery[Criteria]]] 

}


trait PreparedQueryDB[
  F[+_],
  Env,
  Criteria,
  Error
]
{

  def newId(
    implicit env: Env
  ): F[PreparedQuery.Id]


  def save(
    pq: PreparedQuery[Criteria]
  )(
    implicit env: Env
  ): F[Either[Error,PreparedQuery[Criteria]]]


  def delete(
    id: PreparedQuery.Id
  )(
    implicit env: Env
  ): F[Option[PreparedQuery[Criteria]]]


  def get(
    id: PreparedQuery.Id
  )(
    implicit env: Env
  ): F[Option[PreparedQuery[Criteria]]]


  def query(
    query: PreparedQuery.Query
  )(
    implicit env: Env
  ): F[Seq[PreparedQuery[Criteria]]]

}
