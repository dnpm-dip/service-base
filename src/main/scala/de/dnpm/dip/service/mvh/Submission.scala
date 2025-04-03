/*
package de.dnpm.dip.service.mvh


import java.time.LocalDateTime
import de.dnpm.dip.coding.Coding
import de.dnpm.dip.model.{
  Id,
  Period,
  PatientRecord,
  Site
}
//import de.dnpm.dip.model.NGSReport
import play.api.libs.json.{
  Json,
  Format,
  Reads,
  OWrites
}


final case class Submission[T]
(
  record: T,
  metadata: Option[Metadata]
)


object Submission
{

  import play.api.libs.json.{
    JsPath,
    JsObject,
    Reads
  }
  import play.api.libs.functional.syntax._

  implicit def reads[T: Reads]: Reads[Submission[T]] =
    (
      JsPath.read[T] and
      (JsPath \ "metadata").readNullable[Metadata]
    )(
      Submission(_,_)
    )

 object Schemas {

    import json.{
      Json,
      Schema
    }

    implicit val submissionTypeSchema: Schema[SubmissionType.Value] =
      Json.schema[SubmissionType.Value]
        .toDefinition("MVH_SubmissionType")

    implicit val ttanIdSchema: Schema[Id[TransferTAN]] =
      Schema.`string`.asInstanceOf[Schema[Id[TransferTAN]]]
        .toDefinition("TransferTAN")

    implicit val researchConsentSchema: Schema[ResearchConsent] =
      Schema.`object`.Free[JsObject]()
        .asInstanceOf[Schema[ResearchConsent]]
        .toDefinition("ResearchConsent")


    implicit val metadataSchema: Schema[Metadata] =
      Json.schema[Metadata]
        .toDefinition("MVH_Metadata")

    implicit def schema[T <: Product with PatientRecord](  
//    implicit def schema[T <: Product](
      implicit sch: Schema[T]
    ): Schema[Submission[T]] =
      (
        sch match {
          case obj: Schema.`object`[T] =>
            obj.withField(
              "metadata",
              Schema[Metadata],
              false
            )

          case _ => ??? // Cannot occur due to type bound T <: Product, but required for exhaustive pattern match
        }
      )
      .asInstanceOf[Schema[Submission[T]]]
  }

}
*/

