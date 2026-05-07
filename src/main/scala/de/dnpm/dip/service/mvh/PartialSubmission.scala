package de.dnpm.dip.service.mvh


import java.time.{
  LocalDate,
  LocalDateTime
}
import cats.data.NonEmptyList
import play.api.libs.json.{
  Json,
  JsPath,
  Reads
}
import play.api.libs.functional.syntax._
import de.dnpm.dip.model.{
  EpisodeOfCare,
  Id,
  Patient,
  PatientRecord,
  Period,
  Reference
}
import de.dnpm.dip.util.json.readsNel


private [mvh] final case class PartialEpisodeOfCare
(
  id: Id[EpisodeOfCare],
  patient: Reference[Patient],
  period: Period[LocalDate]
)
extends EpisodeOfCare
{
  override val diagnoses = None
}
 
object PartialEpisodeOfCare
{
  implicit val reads: Reads[PartialEpisodeOfCare] =
    Json.reads[PartialEpisodeOfCare]
}


/**
 * Light-weight "header" version of a Submission,
 * containing only the necessary data elements to perform filtering by Submission.Filter,
 */
private[mvh] final case class PartialSubmission
(
  metadata: Submission.Metadata,
  submittedAt: LocalDateTime,
  patient: Patient,
  episodesOfCare: NonEmptyList[EpisodeOfCare]
)

private[mvh] object PartialSubmission
{

  implicit def fromSubmission[T <: PatientRecord](sub: Submission[T]): PartialSubmission =
    PartialSubmission(
      sub.metadata,
      sub.submittedAt,
      sub.record.patient,
      sub.record.episodesOfCare
    )


  // Explicit Reads to include tolerant Reads[Submission.Metadata],
  // required for backwards compatibility with data sets potentially containing 
  // structurally invalid Consent resources
  implicit val reads: Reads[PartialSubmission] =
    (
      (JsPath \ "metadata").read(Submission.tolerantReads.metadata) and
      (JsPath \ "submittedAt").read[LocalDateTime] and
      (JsPath \ "patient").read[Patient] and
      (JsPath \ "episodesOfCare").read[NonEmptyList[PartialEpisodeOfCare]]
    )(
      PartialSubmission(_,_,_,_)
    )

}

