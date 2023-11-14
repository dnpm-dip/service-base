package de.dnpm.dip.service.query


import java.util.UUID.randomUUID
import scala.util.Either
import scala.collection.concurrent.{
  Map,
  TrieMap
}
import cats.Applicative



class InMemPreparedQueryDB[
  F[+_],
  C[M[_]] <: Applicative[M],
  Criteria
]
extends PreparedQueryDB[
  F,C[F],Criteria,String
]
{

  import scala.util.chaining._
  import cats.syntax.applicative._
  import cats.syntax.either._


  private val pqs: Map[PreparedQuery.Id,PreparedQuery[Criteria]] =
    TrieMap.empty


  override def newId(
    implicit env: C[F]
  ): F[PreparedQuery.Id] =
    PreparedQuery.Id(randomUUID.toString)
      .pure


  override def save(
    pq: PreparedQuery[Criteria]
  )(
    implicit env: C[F]
  ): F[Either[String,PreparedQuery[Criteria]]] =
    pq.tap(
      p => pqs += (pq.id -> p)
    )
    .asRight[String]
    .pure


  override def delete(
    id: PreparedQuery.Id
  )(
    implicit env: C[F]
  ): F[Option[PreparedQuery[Criteria]]] =
    pqs.get(id)
      .tapEach(_ => pqs -= id)
      .headOption
      .pure


  override def get(
    id: PreparedQuery.Id
  )(
    implicit env: C[F]
  ): F[Option[PreparedQuery[Criteria]]] =
    pqs.get(id)
      .pure


  override def query(
    query: PreparedQuery.Query
  )(
    implicit env: C[F]
  ): F[Seq[PreparedQuery[Criteria]]] =
    pqs.values
      .filter(
        pq =>
          query.querier.fold(true)(_ == pq.querier)
      )
      .toSeq
      .pure


}
