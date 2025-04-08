package de.dnpm.dip.service


import play.api.libs.json.{
  JsPath,
  JsObject,
  Reads
}
import de.dnpm.dip.service.mvh.{
  ResearchConsent,
  Submission
}


final case class DataUpload[T]
(
  record: T,
  metadata: Option[Submission.Metadata]
)


object DataUpload
{

  import play.api.libs.functional.syntax._

  implicit def reads[T: Reads]: Reads[DataUpload[T]] =
    (
      JsPath.read[T] and
      (JsPath \ "metadata").readNullable[Submission.Metadata]
    )(
      DataUpload(_,_)
    )

  import de.dnpm.dip.model.json.BaseSchemas

  object Schemas extends BaseSchemas
  {

    import json.{
      Json,
      Schema
    }

    implicit val submissionTypeSchema: Schema[Submission.Type.Value] =
      Json.schema[Submission.Type.Value]
        .toDefinition("MVH_SubmissionType")

    implicit val researchConsentSchema: Schema[ResearchConsent] =
      Schema.`object`.Free[JsObject]()
        .asInstanceOf[Schema[ResearchConsent]]
        .toDefinition("ResearchConsent")

    implicit val metadataSchema: Schema[Submission.Metadata] =
      Json.schema[Submission.Metadata]
        .toDefinition("MVH_Metadata")

    implicit def schema[T <: Product](
      implicit sch: Schema[T]
    ): Schema[DataUpload[T]] =
      sch.addOptField(
        "metadata",
        Schema[Submission.Metadata],
      )
      .asInstanceOf[Schema[DataUpload[T]]]

  }

}
