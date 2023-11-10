package de.dnpm.dip.service.query



import de.dnpm.dip.coding.Coding
import de.dnpm.dip.model.{
  Id,
  Patient,
  Site
}



trait ResultSet[PatientRecord,Criteria]
{
  self =>

  import scala.util.chaining._


  type Summary <: ResultSet.Summary


  val id: Query.Id

  def summary: Summary

  def patientMatches(
    f: Patient => Boolean
  ): Seq[PatientMatch[Criteria]]

  def patientMatches: Seq[PatientMatch[Criteria]] =
    patientMatches(_ => true)

  def patientRecord(
    patId: Id[Patient]
  ): Option[PatientRecord] 


  protected[query] def cohort: Seq[PatientRecord]

}



object ResultSet
{

  trait Summary
  {
    val id: Query.Id

    def numPatients: Int

    def patientFilter: PatientFilter

    def siteDistribution: Seq[ConceptCount[Coding[Site]]]
  }

}
