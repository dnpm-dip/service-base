package de.dnpm.dip.service.mvh


import java.time.LocalDateTime
import de.dnpm.dip.coding.Coding
import de.dnpm.dip.model.{
  Id,
  Period,
  Site,
  TransferTAN
}
import de.dnpm.dip.model.NGSReport.SequencingType
import play.api.libs.json.{
  Json,
  JsObject,
  Format,
  Reads,
  OWrites
}


final case class SubmissionReport
(
  createdAt: LocalDateTime,
  site: Coding[Site],
  useCase: UseCase.Value,
  transferTAN: Id[TransferTAN],
  submissionType: SubmissionType.Value,
  sequencingType: Option[SequencingType.Value],
  qcPassed: Boolean
)

object SubmissionReport
{
  implicit val formatSeqType: Format[SequencingType.Value] =
    Json.formatEnum(SequencingType)

  implicit val reads: Reads[SubmissionReport] =
    Json.reads[SubmissionReport]

  implicit val writes: OWrites[SubmissionReport] =
    Json.writes[SubmissionReport]


  final case class Filter(
    creationPeriod: Option[Period[LocalDateTime]] = None
  )

}

