package de.dnpm.dip.service.mvh


import java.time.LocalDateTime
import de.dnpm.dip.model.{
  Id,
  Period,
  PatientRecord,
}
import play.api.libs.json.{
  Json,
  Format,
  Reads,
  OWrites
}



sealed trait TransferTAN  // Transfer Transaction Number (Transfer-Vorgangs-Nummer)


final case class Submission[T <: PatientRecord]
(
  record: T,
  metadata: Submission.Metadata,
  submittedAt: LocalDateTime
)


object Submission
{

  object Type extends Enumeration
  {
    val Initial    = Value("initial")
    val Addition   = Value("addition")
    val Correction = Value("correction")
    val FollowUp   = Value("followup")
  
    implicit val format: Format[Value] =
      Json.formatEnum(this)
  }


  final case class Metadata
  (
    submissionType: Type.Value,
    transferTAN: Id[TransferTAN],
    modelProjectConsent: ModelProjectConsent,
    researchConsents: Option[List[ResearchConsent]]
  )


  object Metadata
  {
    implicit val readsMetadata: Reads[Metadata] =
      Json.reads[Metadata]
  
    implicit val writesMetadata: OWrites[Metadata] =
      Json.writes[Metadata]
  }


  final case class Filter(
    submissionPeriod: Option[Period[LocalDateTime]] = None
  )

  import play.api.libs.json.JsPath
  import play.api.libs.functional.syntax._

  implicit def reads[T <: PatientRecord: Reads]: Reads[Submission[T]] =
    (
      JsPath.read[T] and
      (JsPath \ "metadata").read[Metadata] and
      (JsPath \ "submittedAt").read[LocalDateTime]
    )(
      Submission(_,_,_)
    )

  implicit def writes[T <: PatientRecord: OWrites]: OWrites[Submission[T]] =
    (
      JsPath.write[T] and
      (JsPath \ "metadata").write[Metadata] and
      (JsPath \ "submittedAt").write[LocalDateTime]
    )(
      unlift(Submission.unapply[T](_))
    )

}


/*
final case class Submission[T <: PatientRecord]
(
  record: T,
  metadata: Metadata,
  submittedAt: LocalDateTime
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

