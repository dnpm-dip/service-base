package de.dnpm.dip.service.mvh 


import java.time.{
  LocalDate,
  LocalDateTime
}
import java.time.temporal.ChronoUnit.YEARS
import de.dnpm.dip.coding.{
  CodedEnum,
  Coding,
  DefaultCodeSystem
}
import de.dnpm.dip.model.OpenEndPeriod
import play.api.libs.json.{
  Json,
  JsObject,
  JsPath,
  Format,
  OFormat,
  Reads
}
import play.api.libs.functional.syntax._


object Consent
{

  object Subject extends Enumeration
  {
    val IndexPatient    = Value("index-patient")
    val NonIndexPatient = Value("non-index-patient")

    implicit val format: Format[Value] =
      Json.formatEnum(this)
  }

  object Category extends Enumeration
  {
    val ModelProject = Value("mv-consent")
    val Research     = Value("research-consent")

    implicit val format: Format[Value] =
      Json.formatEnum(this)
  }


  final case class Provision[T]
  (
    date: LocalDate,
    purpose: T,
    `type`: Provision.Type.Value
  )

  object Provision
  {

    object Type extends Enumeration
    { 
      val Permit = Value("permit")
      val Deny   = Value("deny")

      implicit val format: Format[Value] =
        Json.formatEnum(this)
    }

    implicit def format[T: Format]: OFormat[Provision[T]] =
      Json.format[Provision[T]]

  }

}


final case class ModelProjectConsent
(
  version: String,
  date: Option[LocalDate],
  provisions: List[Consent.Provision[ModelProjectConsent.Purpose.Value]]
)

object ModelProjectConsent
{

  object Purpose extends Enumeration
  {
    val Sequencing         = Value("sequencing")
    val Reidentification   = Value("reidentification")
    val CaseIdentification = Value("case-identification")

    implicit val format: Format[Value] =
      Json.formatEnum(this)
  }

  implicit val format: OFormat[ModelProjectConsent] =
    Json.format[ModelProjectConsent]

}


// Wrapper object around a FHIR Consent JSON resource
// with "projection methods" of the necessary attributes from the underlying JSON data.
// This avoids explicitly binding to some bad FHIR DTO library (e.g. HAPI FHIR) or
// having to define DTOs on our own for the bloated/convoluted structure typical of FHIR.
final case class ResearchConsent(value: JsObject) extends AnyVal
{

  def date: Option[LocalDate] =
    (value \ "dateTime").asOpt[LocalDateTime].map(_.toLocalDate)

  def provision(code: String): Option[ResearchConsent.Provision] = {

    val topLevelProvision =
      (value \ "provision").validate[ResearchConsent.Provision].asOpt 
   
    topLevelProvision.filter(_ hasCode code)
      .orElse(
        topLevelProvision.flatMap(_.provision.flatMap(_.find(_ hasCode code)))
      )
     
  }

  def permits(code: String): Boolean =
    provision(code).exists {
      p =>
        lazy val today = LocalDate.now

        (p.`type` == Consent.Provision.Type.Permit) &&
        (p.period.start.toLocalDate isAfter today.minus(5,YEARS)) &&
        (p.period.end.exists(_.toLocalDate isAfter today) || p.period.end.isEmpty)
    }

  def isGiven: Boolean =
    permits(ResearchConsent.PATDAT_STORE_AND_USE) || 
      (permits(ResearchConsent.MDAT_STORE_AND_PROCESS) && permits(ResearchConsent.MDAT_RESEARCH_USE))
}


object ResearchConsent
{

  final case class CodeableConcept[T]
  (
    coding: List[Coding[T]]
  )

  final case class Provision
  (
    `type`: Consent.Provision.Type.Value,
    period: OpenEndPeriod[LocalDateTime],
    code: Option[List[CodeableConcept[Any]]],
    provision: Option[List[Provision]]
  )
  {
    def hasCode(c: String): Boolean =
      this.code.exists(_.exists(_.coding.exists(_.code.value == c)))
  }

  implicit def readCodeableConcept[T](implicit rc: Reads[Coding[T]]): Reads[CodeableConcept[T]] =
    Json.reads[CodeableConcept[T]]

  // Explicit Reads required because of recursive nature of "Provision" (sub-provisions)
  implicit val readProvision: Reads[Provision] =
    (
      (JsPath \ "type").read[Consent.Provision.Type.Value] and
      (JsPath \ "period").read[OpenEndPeriod[LocalDateTime]] and
      (JsPath \ "code").readNullable[List[CodeableConcept[Any]]] and
      (JsPath \ "provision").lazyReadNullable(Reads.of[List[Provision]])
    )(
      Provision(_,_,_,_)
    )


  object ReasonMissing
  extends CodedEnum("dnpm-dip/mvh/research-consent/reason-missing")
  with DefaultCodeSystem
  {

    val PatientInability     = Value("patient-inability")
    val PatientRefusal       = Value("patient-refusal")
    val NonReturnedConsent   = Value("consent-not-returned")
    val OtherPatientReason   = Value("other-patient-reason")
    val TechnicalIssues      = Value("technical-issues")
    val OrganizationalIssues = Value("organizational-issues")

    override val display =
      Map(
        PatientInability     -> "Einwilligung durch den Patienten nicht möglich",
        PatientRefusal       -> "Einwilligung vom Patienten abgelehnt",
        NonReturnedConsent   -> "Einwilligung vom Patienten nicht abgegeben", 
        OtherPatientReason   -> "Anderer Patienten-bedingter Grund", 
        TechnicalIssues      -> "Consent aus technischen Gründen nicht verfügbar", 
        OrganizationalIssues -> "Consent aus organisatorischen Gründen nicht verfügbar"
      )

    implicit val format: Format[Value] =
      Json.formatEnum(this)
  }


  val MDAT_STORE_AND_PROCESS = "2.16.840.1.113883.3.1937.777.24.5.3.7"
  val MDAT_RESEARCH_USE      = "2.16.840.1.113883.3.1937.777.24.5.3.8"
  val PATDAT_STORE_AND_USE   = "2.16.840.1.113883.3.1937.777.24.5.3.1"

  implicit val format: Format[ResearchConsent] =
    Json.valueFormat[ResearchConsent]

}
