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

//  def patientMatches: Seq[PatientMatch[Criteria]]

  def patientMatches: Collection[PatientMatch[Criteria]]

/*
  def patientMatches(
    offset: Option[Int] = None,
    limit: Option[Int] = None,
  ): Seq[PatientMatch[Criteria]] =
    self.patientMatches
      .pipe(
        seq =>
          offset match {
            case Some(n) => seq.drop(n)
            case None    => seq
          }
      )
      .pipe(
        seq =>
          limit match {
            case Some(n) => seq.take(n)
            case None    => seq
          }
      )
*/


  def patientRecord(
    patId: Id[Patient]
  ): Option[PatientRecord] 

/*
  protected[query] def withFilter(
    f: PatientRecord => Boolean
  ): self.type
*/

  protected[query] def cohort: Seq[PatientRecord]

}



object ResultSet
{

  trait Summary
  {
    val id: Query.Id

    def numPatients: Int

    def siteDistribution: Seq[ConceptCount[Coding[Site]]]
  }

}
