package de.dnpm.dip.service.mvh


import java.io.{
  File,
  FileWriter
}
import java.time.LocalDateTime
import scala.util.Random
import scala.util.chaining._
import scala.util.Using
import org.scalatest.flatspec.AnyFlatSpec
import json.{
  Json,
  Schema
}
import org.scalatest.matchers.must.Matchers._
import json.schema.Version._
import com.github.andyglow.jsonschema.AsPlay._
import play.api.libs.json.Json.{
  prettyPrint,
  toJson
}
import de.dnpm.dip.model.{
  Id,
  NGSReport,
  Site
}
import de.dnpm.dip.model.json.BaseSchemas
import de.ekut.tbi.generators.Gen


trait Schemas extends BaseSchemas
{
  implicit val submissionReportSchema: Schema[SubmissionReport] =
    Json.schema[SubmissionReport]
}


class JsonSchemaTests extends AnyFlatSpec with Schemas 
{

  import SubmissionReport._

  System.setProperty("dnpm.dip.site","UKT:Tübingen")

  implicit val rnd: Random = new Random

  implicit val genDataSubmissionReport: Gen[SubmissionReport] =
    for { 
      ttan <- Gen.uuidStrings
      useCase <- Gen.`enum`(UseCase)
      submissionType <- Gen.`enum`(SubmissionType)
      seqType <- Gen.`enum`(NGSReport.SequencingType)
    } yield SubmissionReport(
      LocalDateTime.now,
      Site.local,      
      useCase,
      Id(ttan),
      submissionType,
      Some(seqType),
      true
    )



  "JSON Schema derivation for DataSubmissionReport" must "have worked" in {

    Schema[SubmissionReport].asPlay(Draft12("DataSubmissionReport"))
      .pipe(prettyPrint(_))
      .tap { js =>
        Using(new FileWriter(new File("/home/lucien/MVH-DataSubmissionReport-Schema.json"))){
          _ write js
        }
      }
//      .tap(println(_))

    succeed
  }


  "JSON example generation for DataSubmissionReport" must "have worked" in {

    Gen.of[SubmissionReport].next
      .pipe(toJson(_))
      .pipe(prettyPrint)
//      .tap(println)

    succeed
  }

}
