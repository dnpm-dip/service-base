package de.dnpm.dip.service.mvh


import java.time.LocalDateTime
import de.dnpm.dip.coding.Coding
import de.dnpm.dip.model.{
  HealthInsurance,
  History,
  Id,
  Period,
  Patient,
  PatientRecord,
  Site
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
    id: Id[TransferTAN],
    submittedAt: LocalDateTime,
    patient: Id[Patient],
    status: History[Report.Status],
    site: Coding[Site],
    useCase: UseCase.Value,
    `type`: Type.Value,
    healthInsuranceType: Coding[HealthInsurance.Type.Value]
  )

  object Report
  {

    final case class Status
    (
      value: Status.Value,
      datetime: LocalDateTime
    )

    object Status extends Enumeration
    { 
      val Unsubmitted            = Value("unsubmitted")
//      val RequestedForProcessing = Value("requested-for-processing")
      val Submitted              = Value("submitted")

      implicit val formatValue: Format[Value] =
        Json.formatEnum(this)

      implicit val format: OFormat[Status] =
        Json.format[Status]
    }


    final case class Filter(
      period: Option[Period[LocalDateTime]] = None,
      status: Option[Set[Status.Value]] = None 
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
    transferTAN: Option[Set[Id[TransferTAN]]] = None,
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

