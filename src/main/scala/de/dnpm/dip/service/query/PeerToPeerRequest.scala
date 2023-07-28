package de.dnpm.dip.service.query


import java.time.Instant
import de.dnpm.dip.coding.Coding
import de.dnpm.dip.model.{
  Id,
  Patient,
  Site,
  Snapshot
}
import play.api.libs.json.{
  Json,
  Format,
  Writes,
  Reads
}


trait PeerToPeerRequest
{
  type ResultType

  val origin: Coding[Site]
  val querier: Querier
}


final case class PeerToPeerQuery[Parameters,PatientRecord]
(
  origin: Coding[Site],
  querier: Querier,
  parameters: Parameters,
)
extends PeerToPeerRequest
{
  type ResultType = Seq[Snapshot[PatientRecord]]
}


final case class PatientRecordRequest[PatientRecord]
(
  origin: Coding[Site],
  querier: Querier,
  patient: Id[Patient],
  snapshot: Option[Id[Snapshot[PatientRecord]]]
)
extends PeerToPeerRequest
{
  type ResultType = Snapshot[PatientRecord]
}



/*
final case class PeerToPeerRequest[+T]
(
  origin: Coding[Site],
  querier: Option[Querier],
  body: T,
  submittedAt: Instant = Instant.now
)


object PeerToPeerRequest
{

  implicit def writes[T: Writes] =
    Json.writes[PeerToPeerRequest[T]]

  implicit def reads[T: Reads] =
    Json.reads[PeerToPeerRequest[T]]

}
*/
