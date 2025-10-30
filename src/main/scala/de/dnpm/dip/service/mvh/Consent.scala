package de.dnpm.dip.service.mvh 


import java.time.{
  LocalDate,
  LocalDateTime,
  LocalTime
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
  Reads,
  Writes
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


// Wrapper object around a FHIR Consent JSON resource.
// This avoids explicitly binding to some bad FHIR DTO library (e.g. HAPI FHIR) or
// having to define DTOs on our own for the bloated/convoluted structure typical of FHIR.
final case class BroadConsent(value: JsObject) extends AnyVal

object BroadConsent
{

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

  val researchProvisions =
    Set(
      MDAT_STORE_AND_PROCESS,
      MDAT_RESEARCH_USE,
      PATDAT_STORE_AND_USE
    )


  final case class CodeableConcept[T](coding: List[Coding[T]])
  {
    def codings = coding
  }

  final case class Provision
  (
    `type`: Consent.Provision.Type.Value,
    period: OpenEndPeriod[LocalDateTime],
    private val code: Option[List[CodeableConcept[Any]]],
    private val provision: Option[List[Provision]]
  )
  {
    def codes = code.getOrElse(List.empty)

    def provisions = provision.getOrElse(List.empty)

    def hasCode(c: String): Boolean =
      this.codes.exists(_.codings.exists(_.code.value == c))

    def isPermitted: Boolean = {
      lazy val today = LocalDate.now
    
      (`type` == Consent.Provision.Type.Permit) &&
      (period.start.toLocalDate isAfter today.minus(5,YEARS)) &&
      (period.end.exists(_.toLocalDate isAfter today) || period.end.isEmpty)
    }
    
  }

  // "View" DTO for use as a "projection" of the necessary data
  // in a raw JSON FHIR Consent within a BroadConsent instance,
  // for minimum syntax check and in Consent processing
  final case class View
  (
    dateTime: LocalDateTime,
    provision: Provision
  )
  {
    def date = dateTime.toLocalDate

    def provision(code: String): Option[BroadConsent.Provision] =
      Option.when(provision hasCode code)(provision)
        .orElse(
          provision.provisions.find(_ hasCode code)
        )      

  }


  def permitsResearchUse(consents: List[BroadConsent]): Boolean =
    consents
      .map(rc => Json.fromJson[View](rc.value).get) // .get safe here as incorrect JSON consent would be denied on upload/deserialization
      .foldLeft(Map.empty[String,Boolean]){ 
        (acc,consent) =>
          researchProvisions.foldLeft(acc){ 
            (acc2,code) => acc2.updatedWith(code){
              case denied @ Some(false) => denied   // Short-circuit when denied
              case _ => consent.provision(code).map(_.isPermitted)
            }
          }
      }
      .values
      .exists(_ == true)


  def permitsResearchUse(consent: BroadConsent, consents: BroadConsent*): Boolean =
    permitsResearchUse(consent :: consents.toList)


  implicit def readCodeableConcept[T](implicit rc: Reads[Coding[T]]): Reads[CodeableConcept[T]] =
    Json.reads[CodeableConcept[T]]


  // Custom "tolerant" Reads for LocalDateTime to handle both dateTimes and mere dates in FHIR Consent resources
  private val tolerantDateTime =
    Reads.of[LocalDateTime] orElse Reads.of[LocalDate].map(_ atTime LocalTime.MIN)
      
  private val tolerantPeriod =
    (
      (JsPath \ "start").read(tolerantDateTime) and
      (JsPath \ "end").readNullable(tolerantDateTime)
    )(
      OpenEndPeriod(_,_)
    )


  // Explicit Reads required because of recursive nature of "Provision" (sub-provisions)
  implicit val readProvision: Reads[Provision] =
    (
      (JsPath \ "type").read[Consent.Provision.Type.Value] and
      (JsPath \ "period").read(tolerantPeriod) and
      (JsPath \ "code").readNullable[List[CodeableConcept[Any]]] and
      (JsPath \ "provision").lazyReadNullable(Reads.of[List[Provision]])
    )(
      Provision(_,_,_,_)
    )

  implicit val readView: Reads[View] =
    (
      (JsPath \ "dateTime").read(tolerantDateTime) and
      (JsPath \ "provision").read[Provision]
    )(
      View(_,_)
    )    


  implicit val writes: Writes[BroadConsent] =
    Json.valueWrites[BroadConsent]

  // Decorator Reads[BroadConsent]:
  // Try to read the input JSON as a View to have direct error feedback on Consent resources unusable for post-processing,
  // but then return the raw JSON object wrapped in BroadConsent
  implicit val reads: Reads[BroadConsent] =
    (
      JsPath.read[View] and
      JsPath.read[JsObject]
    )(
      (_,js) => BroadConsent(js)
    )

}
