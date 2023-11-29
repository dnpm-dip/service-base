package de.dnpm.dip.service.query



import de.dnpm.dip.coding.Coding
import de.dnpm.dip.model.{
  Id,
  Patient,
  Gender,
  Interval,
  Site
}



trait ResultSet[PatientRecord,Criteria]
{
  self =>

  import scala.util.chaining._
 
  type Summary <: ResultSet.Summary


  val id: Query.Id

  def summary(
    f: PatientRecord => Boolean = _ => true
  ): Summary


  def patientMatches(
    f: PatientRecord => Boolean = _ => true
  ): Seq[PatientMatch[Criteria]]


  def patientRecord(
    patId: Id[Patient]
  ): Option[PatientRecord] 


  protected[query] def cohort: Seq[PatientRecord]

}


object ResultSet
{

  trait Distributions 
  {
    def gender: Seq[ConceptCount[Coding[Gender.Value]]]
    def age: Seq[ConceptCount[Interval[Int]]]
    def site: Seq[ConceptCount[Coding[Site]]]
  }


  trait Summary
  {

    type DistributionsType <: Distributions

    val id: Query.Id

    def numPatients: Int

    def distributions: DistributionsType

  }

}

