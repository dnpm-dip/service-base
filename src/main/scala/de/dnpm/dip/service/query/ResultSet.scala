package de.dnpm.dip.service.query



trait ResultSet
{
  self =>

  type BuilderFrom[T] = (Query.Id, Seq[T]) => self.type

  type Summary <: ResultSet.Summary


  val id: Query.Id

  val summary: Summary

  val patients: Seq[PatientInfo]

}


object ResultSet
{

  trait Summary
  {
    val id: Query.Id

    val numPatients: Int
  }

}
