package de.dnpm.dip.service.validation


import java.time.Instant
import de.dnpm.dip.model.{
  Id,
  Patient
}
import play.api.libs.json.{
  Json,
  OWrites
}
import Issue.Severity
import Severity._



final case class ValidationInfo
(
  id: Id[Patient],
  issues: Map[String,Int],
//  issues: Map[Severity.Value,Int],
  createdAt: Instant
)


object ValidationInfo
{
  implicit val format: OWrites[ValidationInfo] =
    Json.writes[ValidationInfo]


  // For Ordering:
  // Cpnvert ValidationInfo to tuple of (Num. Errors, Num. Warnings, Num. Infos) and compare these.
  // This ensures that a ValidationInfo with even just 1 Error (1,_,_) is ordered higher
  // than one with many more warnings but no error say (0,42,_)
  implicit val ordering: Ordering[ValidationInfo] =
    Ordering[(Int,Int,Int)].on(
      info =>
        (
          info.issues.getOrElse(Error.toString,0),
          info.issues.getOrElse(Warning.toString,0),
          info.issues.getOrElse(Info.toString,0)
        )
    )

}
