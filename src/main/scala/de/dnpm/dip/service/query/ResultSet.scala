package de.dnpm.dip.service.query


/*
trait ResultSet
{
  self =>

  type BuilderFrom[T] = (Query.Id, Seq[T]) => self.type

  type Summary <: ResultSet.Summary


  val id: Query.Id

  val summary: Summary

  val patients: Seq[PatientInfo]

}
*/

import de.dnpm.dip.model.{
  Id,
  Patient
}

trait ResultSet[PatientRecord,Criteria]
{
  self =>

  type Summary <: ResultSet.Summary


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
