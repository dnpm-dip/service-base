package de.dnpm.dip.service.mvh


import java.time.{
  LocalDate,
  LocalDateTime,
  Year,
}
import java.time.Month._
import de.dnpm.dip.coding.Coding
import de.dnpm.dip.model.{
  ClosedPeriod,
  Site
}
import de.dnpm.dip.service.Distribution
import play.api.libs.json.{
  Json,
  Format,
  OFormat,
}


object Report
{

  type Quarter = ClosedPeriod[LocalDate]

  object Quarter extends Enumeration
  {
    val First  = Value("1")
    val Second = Value("2")
    val Third  = Value("3")
    val Fourth = Value("4")

    private lazy val quarters =
      Map(
        First  -> (JANUARY,MARCH),
        Second -> (APRIL,JUNE),
        Third  -> (JULY,SEPTEMBER),
        Fourth -> (OCTOBER,DECEMBER)
      )

    def apply(
      n: Quarter.Value,
      year: Option[Year] 
    ): Quarter =
      quarters(n) match {
        case (start,end) => ClosedPeriod(
          year.getOrElse(Year.now).atMonth(start).atDay(1),
          year.getOrElse(Year.now).atMonth(end).atEndOfMonth
        )
      }

    implicit val format: Format[Value] =
      Json.formatEnum(this)
  }

  sealed trait Criteria
  final case class ForQuarter(quarter: Quarter.Value, year: Option[Year]) extends Criteria
  final case class ForPeriod(start: LocalDate, end: LocalDate) extends Criteria

}


trait Report
{
  val site: Coding[Site]
  val createdAt: LocalDateTime
  val quarter: Option[Report.Quarter.Value]
  val period: ClosedPeriod[LocalDate]
  val useCase: UseCase.Value
  val submissionTypes: Distribution[Submission.Type.Value]
  val consentRevocations: Option[Map[Consent.Category.Value,Distribution[Consent.Subject.Value]]]
}


final case class BaseReport
(
  site: Coding[Site],
  createdAt: LocalDateTime,
  quarter: Option[Report.Quarter.Value],
  period: ClosedPeriod[LocalDate],
  useCase: UseCase.Value,
  submissionTypes: Distribution[Submission.Type.Value],
  consentRevocations: Option[Map[Consent.Category.Value,Distribution[Consent.Subject.Value]]]
)
extends Report


object BaseReport extends JsonEnumKeyHelpers
{
  implicit val format: OFormat[BaseReport] =
    Json.format[BaseReport]
}
