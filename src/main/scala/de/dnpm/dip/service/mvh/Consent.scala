package de.dnpm.dip.service.mvh 


import java.time.{
  LocalDate,
  LocalDateTime,
}
import java.time.temporal.ChronoUnit.YEARS
import de.dnpm.dip.coding.{
  CodedEnum,
  Coding,
  DefaultCodeSystem
}
import de.dnpm.dip.model.{
  Id,
  OpenEndPeriod,
  Patient,
  Reference
}
import de.dnpm.dip.service.Deidentifier
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




sealed trait BroadConsent
{
  def status: BroadConsent.Status.Value
  def patient: Option[Reference[Patient]]
  def date: LocalDate
  def policyUri: String
  def provision: BroadConsent.Provision

  def provision(code: String): Option[BroadConsent.Provision] =
    Option.when(provision hasCode code)(provision)
      .orElse(
        provision.provisions.find(_ hasCode code)
      )      
}


// Wrapper object around a FHIR Consent JSON resource.
// This avoids explicitly binding to some bad FHIR DTO library (e.g. HAPI FHIR) or
// having to define DTOs on our own for the bloated/convoluted structure typical of FHIR.
private final case class OriginalBroadConsent
(
  json: JsObject
)
extends BroadConsent
{
  private lazy val view =
    Json.fromJson[BroadConsent.View](json).get  // .get safe here as deserializability of JSON into View is checked in Reads[BroadConsent] (see below)
  
  override def status: BroadConsent.Status.Value = view.status

  override def patient: Option[Reference[Patient]] = view.patient

  override def date: LocalDate = view.date

  override def policyUri: String = view.policyUri

  override def provision: BroadConsent.Provision = view.provision

}


// For package internal use only, in order to be able to handle already persisted,
// but possibly erroneous Consent resources in an error-tolerant fashion
private final case class TolerantBroadConsent
(
  json: JsObject
)
extends BroadConsent
{
  // Try reading the JSON consent into View, but handle possible failure
  private lazy val view =
    Json.fromJson[BroadConsent.View](json).asOpt
 
  override def status: BroadConsent.Status.Value =
    view.map(_.status).getOrElse(BroadConsent.Status.EnteredInError)

  override def patient: Option[Reference[Patient]] =
    view.flatMap(_.patient)

  override def date: LocalDate =
    view.map(_.date).getOrElse(LocalDate.now)

  override def policyUri: String =
    view.map(_.policyUri).getOrElse("")

  override def provision: BroadConsent.Provision =
    view.map(_.provision).getOrElse(
      BroadConsent.Provision(
        Consent.Provision.Type.Deny,
        OpenEndPeriod(LocalDate.now,None),
        None,
        None
      )
    )

}


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


  object Status extends Enumeration 
  {
    val Draft          = Value("draft")
    val Proposed       = Value("proposed")
    val Active         = Value("active")
    val Rejected       = Value("rejected")
    val Inactive       = Value("inactive")
    val EnteredInError = Value("entered-in-error")

    implicit val format: Format[Value] =
      Json.formatEnum(this)
  }


  val versions =
    Map(
      "2.16.840.1.113883.3.1937.777.24.2.1790" -> "1.6d", 
      "2.16.840.1.113883.3.1937.777.24.2.1791" -> "1.6f",
      "2.16.840.1.113883.3.1937.777.24.2.2079" -> "1.7.2",
      "2.16.840.1.113883.3.1937.777.24.2.3542" -> "1.7.2 (Eltern und Sorgeberechtigte für Minderjährige v1.1)"
    )


  val MDAT_STORE_AND_PROCESS = "2.16.840.1.113883.3.1937.777.24.5.3.7"
  val MDAT_RESEARCH_USE      = "2.16.840.1.113883.3.1937.777.24.5.3.8"
  val PATDAT_STORE_AND_USE   = "2.16.840.1.113883.3.1937.777.24.5.3.1"

  val researchProvisions =
    Set(
      MDAT_STORE_AND_PROCESS,
      MDAT_RESEARCH_USE,
      PATDAT_STORE_AND_USE
    )


  final case class CodeableConcept[T](private val coding: List[Coding[T]])
  {
    def codings = coding
  }

  final case class Provision
  (
    `type`: Consent.Provision.Type.Value,
    period: OpenEndPeriod[LocalDate],
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
      (period.start isAfter today.minus(5,YEARS)) &&
      (period.end.exists(_ isAfter today) || period.end.isEmpty)
    }
    
  }

  // "View" DTO for use as a "projection" of the necessary data
  // in a raw JSON FHIR Consent within a BroadConsent instance,
  // for minimum syntax check and in Consent processing
  private[mvh] final case class View
  (
    status: BroadConsent.Status.Value,
    patient: Option[Reference[Patient]],
    date: LocalDate,
    policyUri: String,
    provision: Provision
  )
  extends BroadConsent


  // By validation, the List[BroadConsent] would henceforth contain at most 1 element,
  // but keep the implementation here agnostic of this, possibly allowing multiple instances
  def permitsResearchUse(consents: List[BroadConsent]): Boolean =
    consents.forall(_.status == BroadConsent.Status.Active) &&
    consents.foldLeft(Map.empty[String,Boolean]){ 
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


  // By validation, the List[BroadConsent] would henceforth contain at most 1 element
  def permitsResearchUse(consent: BroadConsent): Boolean =
    consent.status == BroadConsent.Status.Active &&
    researchProvisions.map( 
      code => consent.provision(code) match {
        case Some(provision) => provision.isPermitted
        case None => false
      }
    )
    .exists(_ == true)

    
  implicit def readCodeableConcept[T](implicit rc: Reads[Coding[T]]): Reads[CodeableConcept[T]] =
    Json.reads[CodeableConcept[T]]


  // Custom "tolerant" Reads for LocalDateTime to handle either dateTimes or mere dates occurring in FHIR Consent resources
  private val tolerantDate =
    Reads.of[LocalDate] orElse Reads.of[LocalDateTime].map(_.toLocalDate)
      
  private val tolerantPeriod =
    (
      (JsPath \ "start").read(tolerantDate) and
      (JsPath \ "end").readNullable(tolerantDate)
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

    
  private def reference[T](resource: String): Reads[Reference[T]] =
    ((JsPath \ "reference").read[String] orElse (JsPath \ "identifier" \ "value").read[String])
      .map(ref => Reference(Id[T](ref.replace(s"$resource/",""))))

  implicit val readView: Reads[View] = {
    (
      (JsPath \ "status").read[BroadConsent.Status.Value] and
      (JsPath \ "patient").readNullable(reference[Patient]("Patient")) and
      (JsPath \ "dateTime").read(tolerantDate) and
      (JsPath \ "policy"\ 0 \ "uri").read[String] and
      (JsPath \ "provision").read[Provision]
    )(
      View(_,_,_,_,_)
    )    
  }


  implicit val writes: Writes[BroadConsent] =
    Writes { 
      case OriginalBroadConsent(json) => json
      case TolerantBroadConsent(json) => json
      case _: View => ???  // Cannot happen, but required for exhaustive pattern match
    }
    

  // Try to read the input JSON as a View to have direct error feedback on Consent resources unusable for post-processing,
  // but then return the original JSON object wrapped in OriginalBroadConsent
  implicit val reads: Reads[BroadConsent] =
    (
      JsPath.read[View] and
      JsPath.read[JsObject]
    )(
      (_,js) => OriginalBroadConsent(js)
    )

   /*
   * Deidentfier for the BroadConsent:
   * - Remove Consent.id
   * - Replace Consent.patient with a reference using the same id as the MDAT Patient object in the submission
   */
  implicit def deidentifier(
    implicit patient: Id[Patient]
  ): Deidentifier[BroadConsent] = {
    case OriginalBroadConsent(json) =>
      OriginalBroadConsent(json - "id" + ("patient" -> Json.obj("reference" -> s"Patient/${patient}")))

    // Default case: Cannot occur, but required for exhaustive pattern match
    case consent => consent 
  }

}
