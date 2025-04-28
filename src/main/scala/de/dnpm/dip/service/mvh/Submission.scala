package de.dnpm.dip.service.mvh


import java.time.LocalDateTime
import de.dnpm.dip.model.{
  Id,
//  MolecularDiagnostics,
  Period,
  PatientRecord
}
import play.api.libs.json.{
  Json,
  Format,
  OFormat,
  Reads,
  OWrites
}



// Transfer Transaction Number (Transfer-Vorgangs-Nummer)
sealed trait TransferTAN


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


  final case class Report
  (
    submittedAt: LocalDateTime,
    useCase: UseCase.Value,
    `type`: Type.Value,
    transferTAN: Id[TransferTAN],
//    sequencingType: Option[MolecularDiagnostics.Type.Value],
    qcPassed: Boolean
  )

  object Report
  {

    final case class Filter(
      period: Option[Period[LocalDateTime]] = None
    )

    implicit val format: OFormat[Report] =
      Json.format[Report]
  }


  final case class Metadata
  (
    `type`: Type.Value,
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
    transferTANs: Option[Set[Id[TransferTAN]]] = None,
    period: Option[Period[LocalDateTime]] = None
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

