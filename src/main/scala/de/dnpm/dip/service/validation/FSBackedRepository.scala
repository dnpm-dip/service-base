package de.dnpm.dip.service.validation


import java.io.{
  File,
  FileInputStream,
  FileWriter,
  InputStream
}
import scala.util.{
  Either,
  Try,
  Using
}
import scala.util.chaining._
import scala.collection.concurrent.{ 
  Map,
  TrieMap
}
import scala.reflect.ClassTag
import cats.Monad
import cats.syntax.applicative._
import cats.syntax.either._
import play.api.libs.json.{
  Json,
  Format,
  Reads,
  Writes
}
import de.dnpm.dip.util.Logging
import de.dnpm.dip.model.{
  Id,
  Patient
}



class FSBackedRepository[
  F[_],
  PatientRecord <: { def patient: Patient }: Format
]
(
  val dataDir: File
)(
  implicit classTag: ClassTag[PatientRecord]
)
extends Repository[F,Monad[F],PatientRecord]
with Logging
{

  import scala.language.reflectiveCalls

  private val prefix =
    classTag.runtimeClass.getSimpleName 

  private def recordFile(id: Id[Patient]): File =
    new File(dataDir, s"${prefix}_${id.value}.json")

  private def reportFile(id: Id[Patient]): File =
    new File(dataDir, s"ValidationReport_${id.value}.json")


  private def file(record: PatientRecord): File =
    recordFile(record.patient.id)

  private def file(report: ValidationReport): File =
    reportFile(report.patient)


  private def toPrettyJson[T: Writes](t: T): String =
    Json.toJson(t) pipe Json.prettyPrint


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


  private val cache: Map[Id[Patient],(PatientRecord,ValidationReport)] = {

    log.debug(s"Loading persisted data from $dataDir into cache")

    TrieMap.from(
      dataDir.listFiles(
        (_,name) => (name startsWith prefix) && (name endsWith ".json")
      )
      .to(LazyList)
      .map(readJson[PatientRecord])
      .map {
        record =>
          val report =
            readJson[ValidationReport](reportFile(record.patient.id))

          record.patient.id -> (record -> report)
      }
    )
  }

  override def save(
    data: PatientRecord,
    report: ValidationReport
  )(
    implicit env: Monad[F]
  ): F[Either[String,Unit]] = 
    Using.resources(
      new FileWriter(file(data)),
      new FileWriter(file(report))
    ){ 
      (wRecord,wReport) =>
        wRecord.write(toPrettyJson(data))
        wReport.write(toPrettyJson(report))
        cache += report.patient -> (data,report)
        ()
    }
    .asRight[String]
    .pure


  override def ?(
    filter: ValidationService.Filter
  )(
    implicit env: Monad[F]
  ): F[Iterable[(PatientRecord,ValidationReport)]] =
    filter.severities match {

      case Some(severities) =>
        cache.values
          .filter { case (_,report) => severities contains report.maxSeverity }
          .pure

      case None =>
        cache.values.pure

    }
  

  override def ?(
    id: Id[Patient]
  )(
    implicit env: Monad[F]
  ): F[Option[(PatientRecord,ValidationReport)]] =
    cache.get(id)
      .pure


  override def delete(
    id: Id[Patient]
  )(
    implicit env: Monad[F]
  ): F[Either[String,Unit]] =
    Try {
      recordFile(id).delete
      reportFile(id).delete
      cache -= id
    }
    .fold(
      t => t.getMessage.asLeft[Unit],
      _ => ().asRight[String]
    )
    .pure

}
