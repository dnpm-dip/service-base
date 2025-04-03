/*
package de.dnpm.dip.service.mvh


import java.time.LocalDate
import play.api.libs.json.{
  Json,
  JsObject,
  Format,
  OFormat
}
import de.dnpm.dip.coding.{
  Coding,
  CodedEnum,
  DefaultCodeSystem
}
import de.dnpm.dip.model.{
  Id,
  Site
}



object HealthInsuranceType extends Enumeration
{
  val Public     = Value("GKV")
  val Private    = Value("PKV")
  val Supporting = Value("BEI")
  val Other      = Value("other")

  implicit val format: Format[Value] =
    Json.formatEnum(this)
}


trait Submission[Case,MolDx,CarePlan,FollowUp]
{
  val metadata: Submission.Metadata
  val `case`: Case
  val molecularDiagnosticResults: MolDx
  val carePlan: CarePlan
  val followUp: FollowUp
}


object Submission
{

  import Metadata._


final case class Metadata
(
  date: LocalDate,
  `type`: SubmissionType.Value,
  site: Coding[Site],
  healthInsuranceType: HealthInsuranceType.Value,
  tanC: Id[TransferTAN],
//  transferTAN: Id[TransferTAN],
  modelProjectConsent: Consent[ModelProjectPolicies],
  researchConsent: JsObject,  // TODO !!
  boardDate: LocalDate,
  nonInclusionReason: Option[NonInclusionReason.Value]
)  


object Metadata
{

  object Type extends Enumeration
  {
    val Initial    = Value("initial")
    val Addition   = Value("addition")
    val Correction = Value("correction")
    val FollowUp   = Value("follow-up")
    
    implicit val format: Format[Value] =
      Json.formatEnum(this)
  }


  final case class Consent[P <: Product]
  (
    date: LocalDate,
    version: String,
    policies: P
  )

  object ConsentStatus extends Enumeration
  {
    val Accepted = Value("accepted")
    val Rejected = Value("rejected")
    val Revoked  = Value("revoked")

    implicit val formatValue: Format[Value] =
      Json.formatEnum(this)

    implicit val format: Format[ConsentStatus] =
      Json.format[ConsentStatus]
  }

  final case class ConsentStatus
  (
    status: ConsentStatus.Value,
    date: LocalDate
  )
  

  final case class ModelProjectPolicies
  (
    sequencing: List[ConsentStatus],
    reidentification: List[ConsentStatus],
    caseIdentification: List[ConsentStatus]
  )


  object NonInclusionReason
  extends CodedEnum("mv/ausschlussgrund")
  with DefaultCodeSystem
  {
    val TargetedDiagnosticsRecommended = Value("targeted-diagnostics-recommended")
    val PsychosomaticDisease           = Value("psychosomatic")
    val NotRareDisease                 = Value("not-rare-disease")
    val NotGeneticCause                = Value("not-genetic")
    val Other                          = Value("other")

    override val display =
      Map(
        TargetedDiagnosticsRecommended -> "Zieldiagnostik empfohlen",
        PsychosomaticDisease           -> "wahrscheinlich psychosomatische Erkrankung",
        NotRareDisease                 -> "wahrscheinlich häufige Erkrankung",
        NotGeneticCause                -> "wahrscheinlich nicht genetische Ursache",
        Other                          -> "anderer Grund"
      )

    implicit val formatValue: Format[Value] =
      Json.formatEnum(this)
  }


  implicit val formatModelProjectPolicies: OFormat[ModelProjectPolicies] =
    Json.format[ModelProjectPolicies]

  implicit def formatConsent[P <: Product: Format]: OFormat[Consent[P]] =
    Json.format[Consent[P]]

  implicit val format: OFormat[Metadata] =
    Json.format[Metadata]

}

}

object DiagnosticsType extends Enumeration
{
  val Exome           = Value("exome")
  val GenomeLongRead  = Value("genome-long-read ")
  val GenomeShortRead = Value("genome-short-read")
  val Panel           = Value("panel")
  val Single          = Value("single")
  val Array           = Value("array")
  val Karyotyping     = Value("karyotyping")
  val Other           = Value("other")

  implicit val format: Format[Value] =
    Json.formatEnum(this)
}
*/

