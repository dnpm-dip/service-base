package de.dnpm.dip.service.query


import java.time.{
  Instant,
  LocalDateTime
}
import cats.data.EitherNel
import play.api.libs.json.{
  Json,
  Format,
  OFormat,
  Reads
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


  final case class Filter(
    querier: Option[Querier] = None
  )


  implicit def formatId: Format[Id] =
    Json.valueFormat[Id]

  implicit def formatPrepQuery[Criteria: Format]: OFormat[PreparedQuery[Criteria]] =
    Json.format[PreparedQuery[Criteria]]

  implicit def formatCreate[Criteria: Reads]: Reads[Create[Criteria]] =
    Json.reads[Create[Criteria]]

  implicit def formatUpdate[Criteria: Reads]: Reads[Update[Criteria]] =
    Json.reads[Update[Criteria]]

  implicit val formatDelete: Reads[Delete] =
    Json.reads[Delete]

}


trait PreparedQueryOps[
  F[+_],
  Env,
  Criteria,
  Err
]
{

  def !(
    cmd: PreparedQuery.Command[Criteria]
  )(
    implicit
    env: Env,
    querier: Querier
  ): F[Err EitherNel PreparedQuery[Criteria]] 


  def ?(
    id: PreparedQuery.Id
  )(
    implicit
    env: Env,
    querier: Querier
  ): F[Option[PreparedQuery[Criteria]]] 


  def ?(
   filter: PreparedQuery.Filter
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
  Err
]
{

  def newId(
    implicit env: Env
  ): F[PreparedQuery.Id]


  def save(
    pq: PreparedQuery[Criteria]
  )(
    implicit env: Env
  ): F[Either[Err,PreparedQuery[Criteria]]]


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
    filter: PreparedQuery.Filter
  )(
    implicit env: Env
  ): F[Seq[PreparedQuery[Criteria]]]

}
