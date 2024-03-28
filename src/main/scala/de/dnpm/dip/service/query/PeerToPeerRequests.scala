package de.dnpm.dip.service.query



import de.dnpm.dip.coding.Coding
import de.dnpm.dip.model.{
  Id,
  Patient,
  Site,
  Snapshot
}
import de.dnpm.dip.service.PeerToPeerRequest
import play.api.libs.json.{
  Json,
  Format,
  Writes,
  Reads
}



final case class PeerToPeerQuery[Criteria,PatientRecord]
(
  origin: Coding[Site],
  querier: Querier,
  criteria: Criteria
)
extends PeerToPeerRequest
{
  type ResultType = Seq[(Snapshot[PatientRecord],Criteria)]
}

object PeerToPeerQuery
{
  implicit def reads[Criteria: Reads,PatientRecord: Reads]: Reads[PeerToPeerQuery[Criteria,PatientRecord]] =
    Json.reads[PeerToPeerQuery[Criteria,PatientRecord]]

  implicit def writes[Criteria: Writes,PatientRecord: Writes]: Writes[PeerToPeerQuery[Criteria,PatientRecord]] =
    Json.writes[PeerToPeerQuery[Criteria,PatientRecord]]
}



final case class PatientRecordRequest[PatientRecord]
(
  origin: Coding[Site],
  querier: Querier,
  patient: Id[Patient],
  snapshot: Option[Long]
)
extends PeerToPeerRequest
{
  type ResultType = Snapshot[PatientRecord]
}

object PatientRecordRequest
{
  implicit def reads[PatientRecord: Reads]: Reads[PatientRecordRequest[PatientRecord]] =
    Json.reads[PatientRecordRequest[PatientRecord]]

  implicit def writes[PatientRecord: Writes]: Writes[PatientRecordRequest[PatientRecord]] =
    Json.writes[PatientRecordRequest[PatientRecord]]
}

