package de.dnpm.dip.service.mvh


import java.time.LocalDateTime
import de.dnpm.dip.coding.Coding
import de.dnpm.dip.model.{
  HealthInsurance,
  Id,
  NGSReport,
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
    val Test       = Value("test")
    val Initial    = Value("initial")
    val Addition   = Value("addition")
    val Correction = Value("correction")
    val FollowUp   = Value("followup")
  
    implicit val format: Format[Value] =
      Json.formatEnum(this)
  }


  object SequenceType extends Enumeration
  {
    val DNA, RNA = Value
  
    implicit val format: Format[Value] =
      Json.formatEnum(this)
  }


  final case class Report
  (
    id: Id[TransferTAN],
    createdAt: LocalDateTime,
    patient: Id[Patient],
    status: Report.Status.Value,
    site: Coding[Site],
    useCase: UseCase.Value,
    `type`: Type.Value,
    sequencingType: Option[NGSReport.Type.Value],
    sequenceTypes: Option[Set[SequenceType.Value]],
    healthInsuranceType: HealthInsurance.Type.Value,
    consentStatus: Option[Map[Consent.Category.Value,Boolean]],
    reasonResearchConsentMissing: Option[BroadConsent.ReasonMissing.Value]
  )

  object Report extends JsonEnumKeyHelpers
  {

    object Status extends Enumeration
    { 
      val Unsubmitted = Value("unsubmitted")
      val Submitted   = Value("submitted")

      implicit val formatValue: Format[Value] =
        Json.formatEnum(this)
    }

    final case class Filter
    (
      period: Option[Period[LocalDateTime]] = None,
      status: Option[Set[Status.Value]] = None,
      `type`: Option[Set[Type.Value]] = None
    )


    implicit val formatInsType: Format[HealthInsurance.Type.Value] =
      Json.formatEnum(HealthInsurance.Type)

    implicit val formatNgsType: Format[NGSReport.Type.Value] =
      Json.formatEnum(NGSReport.Type)

    implicit val format: OFormat[Report] =
      Json.format[Report]
  }


  final case class Metadata
  (
    `type`: Type.Value,
    transferTAN: Id[TransferTAN],
    modelProjectConsent: ModelProjectConsent,
    researchConsents: Option[List[BroadConsent]],
    reasonResearchConsentMissing: Option[BroadConsent.ReasonMissing.Value]
  )


  object Metadata
  {
    implicit val readsMetadata: Reads[Metadata] =
      Json.reads[Metadata]
  
    implicit val writesMetadata: OWrites[Metadata] =
      Json.writes[Metadata]

  }


  final case class Filter
  (
    transferTAN: Option[Set[Id[TransferTAN]]] = None,
    period: Option[Period[LocalDateTime]] = None
  )

  object Filter
  {
    def apply(period: Period[LocalDateTime]): Filter =
      Filter(None,Some(period))

    def apply(transferTAN: Set[Id[TransferTAN]]): Filter =
      Filter(Some(transferTAN),None)
  }

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

  // Introduced as a (temporary?) workaround:
  // The adapted default Reads for BroadConsent now performs syntactical validation on the JSON,
  // by trying to read the input JSON as a BroadConsent.View, in order to provide error
  // feedback on uploads with erroneous Consent resources.
  // However, in order to retain backward compatibility for already persisted submissions
  // with possibly (as of now) erroneous Consent, a tolerant json.Reads (i.e. which just reads BroadConsent
  // objects as plain JsObject) must be used for Submission.Metadata under the hood here.
  object tolerantReads 
  {

    import play.api.libs.functional.syntax._

    def metadata: Reads[Metadata] =
      (
        (JsPath \ "type").read[Submission.Type.Value] and
        (JsPath \ "transferTAN").read[Id[TransferTAN]] and
        (JsPath \ "modelProjectConsent").read[ModelProjectConsent] and
        (JsPath \ "researchConsents").readNullable(Reads.list(Json.valueReads[UnvalidatedBroadConsent])) and
        (JsPath \ "reasonResearchConsentMissing").readNullable[BroadConsent.ReasonMissing.Value]
      )(
        Submission.Metadata(_,_,_,_,_)
      )
    
    def submission[T <: PatientRecord: Reads]: Reads[Submission[T]] =
      (
        JsPath.read[T] and
        (JsPath \ "metadata").read(metadata) and
        (JsPath \ "submittedAt").read[LocalDateTime]
      )(
        Submission(_,_,_)
      )

  }

}

