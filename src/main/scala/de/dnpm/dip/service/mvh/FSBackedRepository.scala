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
  Writes,
  OWrites
}
import de.dnpm.dip.model.{
  Id,
  Patient,
  PatientRecord,
}


class FSBackedRepository[F[_],T <: PatientRecord: Reads: OWrites](
  dataDir: File
)(
  implicit classTag: ClassTag[T]
)
extends Repository[F,Monad[F],T]
{

  type Env = Monad[F]


  private def mvhRecordFile(id: Id[Patient]): File =
    new File(dataDir,s"MVH_${classTag.runtimeClass.getSimpleName}_${id.value}.json")


  private def toPrettyJson[A: Writes](a: A): String =
    Json.toJson(a) pipe Json.prettyPrint

  private def readAsJson[A: Reads]: InputStream => A =
    Json.parse(_)
      .pipe(Json.fromJson[A](_))
      .pipe(_.get)



  override def save(
    mvhRecord: Submission[T]
  )(
    implicit env: Env
  ): F[Either[String,Unit]] = {
    Using(
      new FileWriter(mvhRecordFile(mvhRecord.record.id))
    ){
      _.write(toPrettyJson(mvhRecord))
    }
    .fold(
      t => t.getMessage.asLeft,
      _ => ().asRight
    )
    .pure
  }


  override def ?(fltr: Submission.Filter)(
    implicit env: Env
  ): F[Iterable[Submission[T]]] =
    dataDir.listFiles(
      (_,name) => (name startsWith "MVH_") && (name endsWith ".json")
    )
    .to(Iterable)
    .map(new FileInputStream(_))
    .map(readAsJson[Submission[T]])
    .filter(fltr)
    .pure


  override def delete(id: Id[Patient])(
    implicit env: Env
  ): F[Either[String,Unit]] = 
    mvhRecordFile(id).delete match {
      case true  => ().asRight[String].pure

      case false => s"Failed to delete Submission for Patient $id".asLeft[Unit].pure
    }

}
