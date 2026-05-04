package de.dnpm.dip.service.query


import java.io.{
  File,
  FileWriter,
  FileInputStream
}
import scala.reflect.ClassTag
import scala.util.{
  Either,
  Try,
  Failure,
}
import scala.collection.concurrent.{
  Map,
  TrieMap
}
import cats.Applicative
import de.dnpm.dip.util.Logging
import de.dnpm.dip.model.{
  Id,
  Patient,
  PatientRecord,
  Snapshot
}
import de.dnpm.dip.service.DataCounts
import QueryService.{
  Saved,
  Deleted
}
import play.api.libs.json.{
  Json,
  Format,
  Reads
}



class FSBackedLocalDB[
  F[_],
  C[M[_]] <: Applicative[M],
  Criteria,
  T <: PatientRecord: Format
](
  val dataDir: File,
  val criteriaMatcher: Criteria => (T => Option[Criteria]),
)(
  implicit classTag: ClassTag[T]
)
extends LocalDB[F,C[F],Criteria,T]
with Logging
{

//  import scala.language.reflectiveCalls
  import scala.util.Using
  import scala.util.chaining._
  import cats.syntax.functor._
  import cats.syntax.applicative._
  import cats.syntax.either._

  private val prefix =
    classTag.runtimeClass.getSimpleName


  private def fileStart(
    patId: Id[Patient]
  ): String =
    s"${prefix}_${patId.value}"

  private def fileName(
    patId: Id[Patient],
    snpId: Long
  ): String =
    s"${fileStart(patId)}_Snapshot_${snpId}.json"

  private def fileOf(
    snp: Snapshot[T]
  ): File =
    new File(dataDir,fileName(snp.data.patient.id,snp.timestamp))


  private def readJson[A: Reads](file: File): A =
    Using.resource(new FileInputStream(file)){
      input =>
       Json.parse(input)
         .pipe(Json.fromJson[A](_))
         .tap(
           _.fold(
             errs => log.error(s"Error(s) occurred parsing file $file: \n${errs.toString}"),
             _ => ()
           )
         )
         .pipe(_.get)
    }


  private val cache: Map[Id[Patient],Snapshot[T]] = {

    log.debug(s"Setting up file-system persistence to directory ${dataDir.getAbsolutePath}")

    if (!dataDir.exists)
      dataDir.mkdirs
        .tap {
          case false =>
            log.warn(
              s"Failed to create directory ${dataDir.getAbsolutePath}. Ensure the executing user has appropriate permissions on the directory."
            )
          case _ => ()
        }
    
    dataDir.listFiles(
      (_,name) => (name startsWith prefix) && (name endsWith ".json")
    )
    .to(LazyList)
    .map(readJson[Snapshot[T]])
    // Lazily accumulate only the latest snapshot of each patient record,
    // instead of using groupyBy(patientId) and then picking the latest snapshot,
    // which requires all to be loaded into memory, whereas with this 
    // implementation, elements can be garbage-collected along the way
    .foldLeft(TrieMap.empty[Id[Patient],Snapshot[T]]){ 
      (acc,snp) =>
        acc.updateWith(snp.data.patient.id){
          case Some(s) =>
            if (snp.timestamp > s.timestamp) Some(snp)
            else Some(s)

          case None => Some(snp)
        }
        acc
    }
    
  }


  override def save(
    dataSet: T
  )(
    implicit env: C[F]
  ): F[Either[String,Saved[T]]] = {
  
    //TODO: Logging
   
    val snp = Snapshot.of(dataSet)

    Using(new FileWriter(fileOf(snp))){
      _.write(
        Json.toJson(snp) pipe Json.stringify
      )
    }
    .map(_ => cache update (dataSet.patient.id,snp))
    .fold(
      _.getMessage.asLeft[Saved[T]],
      _ => Saved(snp).asRight[String]
    )
    .pure

  }


  override def delete(
    patId: Id[Patient],
  )(
    implicit env: C[F]
  ): F[Either[String,Deleted]] = {

    import java.nio.file.Files
    import cats.syntax.traverse._

    log.info(s"Deleting all patient record snapshot files for Patient ${patId.value}")

    dataDir.listFiles(
      (_,name) => name startsWith fileStart(patId)
    )
    .to(LazyList)
    .map(
      file =>
        Try(Files.delete(file.toPath))
          .map(_ => cache -= patId)
          .recoverWith {
            case t =>
              log.error(s"Couldn't delete file ${file.getAbsolutePath}",t)
              Failure(t)
          }

    )
    .sequence
    .fold(
      _ => s"Error(s) occurred deleting data files of Patient ${patId.value}, check the log".asLeft[Deleted],
      _ => Deleted(patId).asRight[String] 
    )
    .pure

  }


  override def ?(
    criteria: Option[Criteria]
  )(
    implicit env: C[F]
  ): F[Either[String,Seq[Query.Match[T,Criteria]]]] = {

    criteria.fold(
      cache.values
        .map(Query.Match(_,criteria))

    ){
      crit =>

        val matcher = criteriaMatcher(crit)
              
        cache.values
          .map(snp => Query.Match(snp,matcher(snp.data)))
          .filter(_.matchingCriteria.isDefined)
    }
    .toSeq
    .pure
    .map(_.asRight[String])

  }


  override def ?(
    patient: Id[Patient],
    snapshot: Option[Long] = None
  )(
    implicit env: C[F]
  ): F[Option[Snapshot[T]]] = {

    //TODO: Logging

    (
      snapshot match {
      
        case Some(snpId) =>
          dataDir.listFiles(
            (_,name) => name == fileName(patient,snpId)
          )
          .headOption
          .map(readJson[Snapshot[T]])

        case None =>
          cache.get(patient)
      
      }
    )
    .pure

  }


  override def dataCounts(
    criteria: Option[DataCounts.Criteria]
  )(
    implicit env: C[F]
  ): F[DataCounts] =
    env.pure {
      cache.foldLeft(
        0 -> criteria.map(_ => 0)
      ){
        case (totalEpisodes -> criteriaMatches, _ -> snapshot) =>
          (
            totalEpisodes + snapshot.data.episodesOfCare.size,
            criteria.flatMap {
              crit => criteriaMatches.map(
                _ + snapshot.data.episodesOfCare.toList.count(eoc => crit.episodeOfCarePeriod contains eoc.period.start)
              )
            }
          )
      }
    }
    .map {
      case (total,matching) => DataCounts(cache.size,total,matching)
    }

}
