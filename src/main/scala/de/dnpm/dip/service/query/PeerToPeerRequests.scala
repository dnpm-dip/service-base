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
  Writes,
  Reads
}


sealed trait Request extends PeerToPeerRequest

final case class FederatedQuery[Criteria,PatientRecord]
(
  origin: Coding[Site],
  querier: Querier,
  criteria: Option[Criteria]
)
extends Request
{
  type ResultType = Seq[Query.Match[PatientRecord,Criteria]]
}


object FederatedQuery
{
  implicit def reads[Criteria: Reads,PatientRecord: Reads]: Reads[FederatedQuery[Criteria,PatientRecord]] =
    Json.reads[FederatedQuery[Criteria,PatientRecord]]

  implicit def writes[Criteria: Writes,PatientRecord: Writes]: Writes[FederatedQuery[Criteria,PatientRecord]] =
    Json.writes[FederatedQuery[Criteria,PatientRecord]]
}



final case class PatientRecordRequest[PatientRecord]
(
  origin: Coding[Site],
  querier: Querier,
  patient: Id[Patient],
  snapshot: Option[Long]
)
extends Request
{
  type ResultType = Option[Snapshot[PatientRecord]]
}

object PatientRecordRequest
{
  implicit def reads[PatientRecord: Reads]: Reads[PatientRecordRequest[PatientRecord]] =
    Json.reads[PatientRecordRequest[PatientRecord]]

  implicit def writes[PatientRecord: Writes]: Writes[PatientRecordRequest[PatientRecord]] =
    Json.writes[PatientRecordRequest[PatientRecord]]
}
