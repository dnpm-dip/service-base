package de.dnpm.dip.service.query


import de.dnpm.dip.model.{
  Id,
  Patient,
  Snapshot
}


trait BaseResultSet[
  PatientRecord <: { val patient: Patient },
  Criteria
]
extends ResultSet[PatientRecord,Criteria]
{
  self =>

  import scala.util.chaining._
  import scala.language.reflectiveCalls
  import ReportingOps._


  val id: Query.Id

  val results: Seq[(Snapshot[PatientRecord],Criteria)]

  protected lazy val records =
    results
      .collect {
        case (Snapshot(record,_),_) => record
      }


  override def patientMatches(
    f: PatientRecord => Boolean
  ): Seq[PatientMatch[Criteria]] =
    results
      .collect {
        case (Snapshot(patRec,_),matchingCriteria) if f(patRec) =>
          PatientMatch.of(
            patRec.patient,
            matchingCriteria
          )
      }


  override def patientRecord(
    patId: Id[Patient]
  ): Option[PatientRecord] =
    records
      .find(_.patient.id == patId)

}

