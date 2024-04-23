package de.dnpm.dip.service.validation


import java.time.YearMonth
import de.dnpm.dip.model.{
  Id,
  Patient
}
import play.api.libs.json.{
  Json,
  OWrites
}


final case class DataValidationInfo
(
  id: Id[Patient],
  numberOfIssues: Map[Issue.Severity.Value,Int]
)


object DataValidationInfo
{
  implicit val format: OWrites[DataValidationInfo] =
    Json.writes[DataValidationInfo]
}

