package de.dnpm.dip.service.validation


import java.time.YearMonth
import de.dnpm.dip.model.{
  Id,
  Patient
}
import play.api.libs.json.{
  Json,
  Writes
}


final case class DataValidationInfo
(
  id: Id[Patient],
  numberOfIssues: Map[ValidationReport.Issue.Severity.Value,Int]
)


object DataValidationInfo
{
  implicit val format: Writes[DataValidationInfo] =
    Json.writes[DataValidationInfo]
}

