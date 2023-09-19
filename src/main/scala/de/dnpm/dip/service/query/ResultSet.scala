package de.dnpm.dip.service.query



import de.dnpm.dip.model.{
  Id,
  Patient
}

//trait ResultSet[PatientRecord,Crit]
trait ResultSet[PatientRecord,Criteria]
{
  self =>

  type Summary <: ResultSet.Summary
//  type Criteria = Crit


  val id: Query.Id

  def summary: Summary

  def patientMatches: Seq[PatientMatch[Criteria]]

  def patientRecord(
    patId: Id[Patient]
  ): Option[PatientRecord] 


  protected[query] def withFilter(
    f: PatientRecord => Boolean
  ): self.type

  protected[query] def cohort: Seq[PatientRecord]

}



object ResultSet
{

  trait Summary
  {
    val id: Query.Id

    val numPatients: Int
  }

}
