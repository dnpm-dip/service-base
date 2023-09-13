package de.dnpm.dip.service.validation


import java.time.Instant
import cats.data.NonEmptyList
import de.dnpm.dip.model.{
  Id,
  Patient
}
import play.api.libs.json.{
  Json,
  Format
}



case class ValidationReport
(
  patient: Id[Patient],
  issues: NonEmptyList[ValidationReport.Issue],
  createdAt: Instant
)
{
  val maxSeverity =
    issues.toList.map(_.severity).max
}


object ValidationReport
{

  def apply(
    patient: Id[Patient],
    issues: NonEmptyList[ValidationReport.Issue]
  ): ValidationReport =
    ValidationReport(patient,issues,Instant.now)

  case class Issue
  (
    severity: Issue.Severity.Value,
    message: String,
    location: Issue.Location
  )

  object Issue
  {

    object Severity extends Enumeration
    {
      val Info    = Value("info")
      val Warning = Value("warning")
      val Error   = Value("error")
      val Fatal   = Value("fatal")
    
      implicit val format: Format[Severity.Value] =
        Json.formatEnum(this)
    }
    
    case class Location
    (
      entryType: String,
      id: String,
      attribute: String
    )


    sealed trait Builder
    {
      def at(loc: Location): Issue
    }   

    private case class BuilderImpl
    (
      sev: Severity.Value,
      msg: String
    )
    extends Builder
    {
      def at(loc: Location): Issue = Issue(sev,msg,loc)
    }


    def Info(msg: String): Builder =
      BuilderImpl(Severity.Info,msg)

    def Warning(msg: String): Builder =
      BuilderImpl(Severity.Warning,msg)

    def Error(msg: String): Builder =
      BuilderImpl(Severity.Error,msg)

    def Fatal(msg: String): Builder =
      BuilderImpl(Severity.Fatal,msg)


    implicit val formatLocation: Format[Location] =
      Json.format[Location]
    
    implicit val format: Format[Issue] =
      Json.format[Issue]

  }


  import de.dnpm.dip.util.json._

  implicit val format: Format[ValidationReport] = 
    Json.format[ValidationReport]

}
