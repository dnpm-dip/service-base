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


  protected var filter: PatientRecord => Boolean =
    _ => true

  override def patientMatches: Seq[PatientMatch[Criteria]] = 
    results
      .collect {
//        case (Snapshot(_,patRec,_),matchingCriteria) if (filter(patRec)) =>
        case (Snapshot(patRec,_),matchingCriteria) if (filter(patRec)) =>
          PatientMatch.of(
            patRec.patient,
            matchingCriteria
          )
      }


  override def patientRecord(
    patId: Id[Patient]
  ): Option[PatientRecord] =
    results.collectFirst {
//      case (Snapshot(_,patRec,_),_)
      case (Snapshot(patRec,_),_) if (patRec.patient.id == patId) => patRec
    }


  override def withFilter(
    f: PatientRecord => Boolean
  ) = {
    filter = f
    self
  }


  override lazy val cohort: Seq[PatientRecord] = 
    results.map(_._1.data)


}

