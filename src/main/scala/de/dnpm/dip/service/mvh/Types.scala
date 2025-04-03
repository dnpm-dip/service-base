package de.dnpm.dip.service.mvh


import java.time.LocalDateTime
import de.dnpm.dip.model.{
  Id,
  Period,
  PatientRecord
}
import play.api.libs.json.{
  Json,
  Format,
  Reads,
  Writes,
  OWrites
}

// Transfer Transaction Number (Transfer-Vorgangs-Nummer)
sealed trait TransferTAN


//-----------------------------------------------------------------------------

object SubmissionType extends Enumeration
{
  val Initial    = Value("initial")
  val Addition   = Value("addition")
  val Correction = Value("correction")
  val FollowUp   = Value("followup")
  
  implicit val format: Format[Value] =
    Json.formatEnum(this)
}

//-----------------------------------------------------------------------------

final case class Metadata
(
  submissionType: SubmissionType.Value,
  transferTAN: Id[TransferTAN],
  modelProjectConsent: ModelProjectConsent,
  researchConsents: Option[List[ResearchConsent]]
)


//-----------------------------------------------------------------------------

object UseCase extends Enumeration
{
  val MTB,RD = Value

  implicit val format: Format[Value] =
    Json.formatEnum(this)
}

object Metadata
{
  implicit val readsMetadata: Reads[Metadata] =
    Json.reads[Metadata]

  implicit val writesMetadata: Writes[Metadata] =
    Json.writes[Metadata]
}


//-----------------------------------------------------------------------------


final case class MVHPatientRecord[T <: PatientRecord]
(
  record: T,
  metadata: Metadata,
  submittedAt: LocalDateTime
)

object MVHPatientRecord
{

  final case class Filter(
    submissionPeriod: Option[Period[LocalDateTime]] = None
  )

  import play.api.libs.json.JsPath
  import play.api.libs.functional.syntax._

  implicit def reads[T <: PatientRecord: Reads]: Reads[MVHPatientRecord[T]] =
    (
      JsPath.read[T] and
      (JsPath \ "metadata").read[Metadata] and
      (JsPath \ "submittedAt").read[LocalDateTime]
    )(
      MVHPatientRecord(_,_,_)
    )

  implicit def writes[T <: PatientRecord: Writes]: OWrites[MVHPatientRecord[T]] =
    (
      JsPath.write[T] and
      (JsPath \ "metadata").write[Metadata] and
      (JsPath \ "submittedAt").write[LocalDateTime]
    )(
      unlift(MVHPatientRecord.unapply[T](_))
    )

}
