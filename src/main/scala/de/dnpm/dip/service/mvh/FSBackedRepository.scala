package de.dnpm.dip.service.mvh


import java.io.{
  File,
  FileInputStream,
  FileWriter,
  InputStream
}
import scala.reflect.ClassTag
import scala.util.chaining._
import scala.util.Using
import cats.Monad
import cats.syntax.applicative._
import cats.syntax.either._
import play.api.libs.json.{
  Json,
  Reads,
  Writes
}
import de.dnpm.dip.model.{
  Id,
  Patient,
  PatientRecord,
}

/*
private[mvh] final case class MVHPatientRecordWithId
(
  patient: Id[Patient],
  report: MVHPatientRecord
)

object MVHPatientRecordWithId
{
  private[mvh] implicit val format: Format[MVHPatientRecordWithId] =
    Json.format[MVHPatientRecordWithId]
}
*/

class FSBackedRepository[F[_],T <: PatientRecord: Reads: Writes](
  dataDir: File
)(
  implicit classTag: ClassTag[T]
)
extends Repository[F,Monad[F],T]
{

  type Env = Monad[F]


  private def mvhRecordFile(id: Id[Patient]): File =
    new File(dataDir,s"MVH_${classTag.runtimeClass.getSimpleName}_${id.value}.json")

//  private def reportFile(id: Id[Patient]): File =
//    new File(dataDir,s"MVHPatientRecord_${id.value}.json")


  private def toPrettyJson[A: Writes](a: A): String =
    Json.toJson(a) pipe Json.prettyPrint

  private def readAsJson[A: Reads]: InputStream => A =
    Json.parse(_)
      .pipe(Json.fromJson[A](_))
      .pipe(_.get)



  override def save(
    mvhRecord: MVHPatientRecord[T]
  )(
    implicit env: Env
  ): F[Either[String,Unit]] = {

    val patId = mvhRecord.record.id
/*
    Using.resources(
      new FileWriter(mvhRecordFile(patId)),
      new FileWriter(reportFile(patId))
    ){
      (recordWriter,reportWriter) =>
        recordWriter.write(toPrettyJson(mvhRecord))
        reportWriter.write(
          toPrettyJson(
            MVHPatientRecordWithId(patId,report)
          )
        )
    }
    .asRight[String]
    .pure
*/    
    Using(
      new FileWriter(mvhRecordFile(patId))
    ){
      _.write(toPrettyJson(mvhRecord))
    }
    .fold(
      t => t.getMessage.asLeft,
      _ => ().asRight
    )
    .pure
  }


  override def ?(fltr: MVHPatientRecord.Filter)(
    implicit env: Env
  ): F[Iterable[MVHPatientRecord[T]]] =
    dataDir.listFiles(
      (_,name) => (name startsWith "MVH_") && (name endsWith ".json")
    )
    .to(Iterable)
    .map(new FileInputStream(_))
    .map(readAsJson[MVHPatientRecord[T]])
    .filter(fltr)
    .pure


  override def delete(id: Id[Patient])(
    implicit env: Env
  ): F[Either[String,Unit]] = 
    mvhRecordFile(id).delete match {
      case true  => ().asRight[String].pure

      case false => s"Failed to delete MVHPatientRecord for Patient $id".asLeft[Unit].pure
    }

}
