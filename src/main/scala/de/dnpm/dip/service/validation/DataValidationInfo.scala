package de.dnpm.dip.service.validation


import java.time.YearMonth
import play.api.libs.json.Json
import de.dnpm.dip.model.{
  Id,
  Patient
}


final case class DataValidationInfo
(
  id: Id[Patient],
  numberOfIssues: Map[ValidationReport.Issue.Severity.Value,Int]
)


object DataValidationInfo
{
  implicit val format = Json.writes[DataValidationInfo]
}

