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

  import scala.language.reflectiveCalls


  val id: Query.Id

  val results: Seq[(Snapshot[PatientRecord],Criteria)]

  def patientMatches(
    f: Patient => Boolean
  ): Seq[PatientMatch[Criteria]] =
    results
      .collect {
        case (Snapshot(patRec,_),matchingCriteria) if f(patRec.patient) =>
          PatientMatch.of(
            patRec.patient,
            matchingCriteria
          )
      }


  override def patientRecord(
    patId: Id[Patient]
  ): Option[PatientRecord] =
    results.collectFirst {
      case (Snapshot(patRec,_),_) if (patRec.patient.id == patId) => patRec
    }


  override lazy val cohort: Seq[PatientRecord] = 
    results.map(_._1.data)


}

