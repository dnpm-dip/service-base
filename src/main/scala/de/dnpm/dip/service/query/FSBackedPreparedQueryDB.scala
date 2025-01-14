package de.dnpm.dip.service.query


import java.io.{
  File,
  FileWriter,
  FileInputStream
}
import java.util.UUID.randomUUID
import scala.util.Either
import scala.collection.concurrent.{
  Map,
  TrieMap
}
import scala.util.Using
import cats.Applicative
import play.api.libs.json.{
  Json,
  Format,
  Reads
}
import de.dnpm.dip.util.Logging


class FSBackedPreparedQueryDB[
  F[+_],
  C[M[_]] <: Applicative[M],
  Criteria: Format
](
  val dataDir: File
)
extends PreparedQueryDB[F,C[F],Criteria,String]
with Logging
{

  import scala.util.chaining._
  import cats.syntax.applicative._
  import cats.syntax.either._


  private def readJson[T: Reads](file: File): T =
    Json.parse(new FileInputStream(file))
      .pipe(Json.fromJson[T](_))
      .tap(
        _.fold(
          errs => log.error(s"Error(s) occurred parsing file $file: \n${errs.toString}"),
          _ => ()
        )
      )
      .pipe(_.get)
      

  private val cache: Map[PreparedQuery.Id,PreparedQuery[Criteria]] = {

    log.debug(s"Setting up persistence of Prepared Queries to directory ${dataDir.getAbsolutePath}")

    if (!dataDir.exists)
      dataDir.mkdirs
        .tap {
          case false =>
            log.warn(
              s"Failed to create directory ${dataDir.getAbsolutePath}. Ensure the executing user has appropriate permissions on the directory."
            )
          case _ => ()
        }

    TrieMap.from(
      dataDir.listFiles(
        (_,name) => (name startsWith "PreparedQuery") && (name endsWith ".json")
      )
      .map(readJson[PreparedQuery[Criteria]])
      .map(q => q.id -> q)
    )

  }


  private def fileName(q: PreparedQuery[Criteria]): String =
    s"PreparedQuery_${q.id.value}.json"

  private def file(q: PreparedQuery[Criteria]): File =
    new File(dataDir,fileName(q))


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
    Using(new FileWriter(file(pq))){ w => 
      w.write(Json.toJson(pq) pipe Json.prettyPrint)
      cache += (pq.id -> pq)
      pq
    }
    .toEither
    .leftMap(_.getMessage)
    .pure


  override def delete(
    id: PreparedQuery.Id
  )(
    implicit env: C[F]
  ): F[Option[PreparedQuery[Criteria]]] =
    cache.remove(id)
      .tap(_.foreach(file(_).delete))
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
