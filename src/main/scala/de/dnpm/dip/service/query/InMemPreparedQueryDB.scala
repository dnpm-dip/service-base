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


  private val cache: Map[PreparedQuery.Id,PreparedQuery[Criteria]] =
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
      p => cache += (pq.id -> p)
    )
    .asRight[String]
    .pure


  override def delete(
    id: PreparedQuery.Id
  )(
    implicit env: C[F]
  ): F[Option[PreparedQuery[Criteria]]] =
    cache.get(id)
      .tapEach(_ => cache -= id)
      .headOption
      .pure


  override def get(
    id: PreparedQuery.Id
  )(
    implicit env: C[F]
  ): F[Option[PreparedQuery[Criteria]]] =
    cache.get(id)
      .pure


  override def query(
    filter: PreparedQuery.Filter
  )(
    implicit env: C[F]
  ): F[Seq[PreparedQuery[Criteria]]] =
    filter.querier.fold(
      cache.values
    )(
      q => cache.values.filter(_.querier == q)
    )
    .toSeq
    .pure

}
