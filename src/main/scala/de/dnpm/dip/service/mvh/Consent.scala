package de.dnpm.dip.service.mvh 


import java.time.{
  LocalDate,
  LocalDateTime
}
import play.api.libs.json.{
  Json,
  JsObject,
  Format,
  OFormat
}


object Consent
{

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
// having to define DTOs with the convoluted structure typical of FHIR on our own.
final case class ResearchConsent(value: JsObject) extends AnyVal
{

  import Consent.Provision.Type._

  def date: Option[LocalDate] =
    (value \ "dateTime").asOpt[LocalDateTime]
      .map(_.toLocalDate)

  def provisionType(code: String): Option[Consent.Provision.Type.Value] =
    for {
      provisions <- (value \ "provision" \ "provision").validate[Seq[JsObject]].asOpt

      `type` <- provisions.collectFirst {
         case provision if (provision \ "code" \\ "coding").exists(coding => (coding \\ "code").exists(_.as[String] == code)) =>
          (provision \ "type").validate[Consent.Provision.Type.Value].getOrElse(Deny)
      }
    } yield `type`

  def permits(code: String): Boolean =
    provisionType(code).exists(_ == Permit)

}


object ResearchConsent
{

  val MDAT_STORE_AND_PROCESS = "2.16.840.1.113883.3.1937.777.24.5.3.7"
  val MDAT_RESEARCH_USE      = "2.16.840.1.113883.3.1937.777.24.5.3.8"
  val PATDAT_STORE_AND_USE   = "2.16.840.1.113883.3.1937.777.24.5.3.1"

  implicit val format: Format[ResearchConsent] =
    Json.valueFormat[ResearchConsent]
}
