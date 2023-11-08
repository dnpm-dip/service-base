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

  val id: Query.Id

  val results: Seq[(Snapshot[PatientRecord],Criteria)]


  import scala.language.reflectiveCalls


  override def patientMatches: Collection[PatientMatch[Criteria]] = 
    Collection(
      results
        .collect {
          case (Snapshot(patRec,_),matchingCriteria) =>
            PatientMatch.of(
              patRec.patient,
              matchingCriteria
            )
        }
    )

  override def patientRecord(
    patId: Id[Patient]
  ): Option[PatientRecord] =
    results.collectFirst {
      case (Snapshot(patRec,_),_) if (patRec.patient.id == patId) => patRec
    }


  override lazy val cohort: Seq[PatientRecord] = 
    results.map(_._1.data)


}

