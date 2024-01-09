package de.dnpm.dip.service.query


import de.dnpm.dip.coding.Coding
import de.dnpm.dip.model.{
  Id,
  Patient,
  Gender,
  Interval,
  Site
}
import play.api.libs.json.{
  Json,
  OWrites
}


trait ResultSet[PatientRecord,Criteria]
{
  self =>

  type SummaryType <: ResultSet.Summary


  val id: Query.Id


  def summary(
    f: PatientRecord => Boolean = _ => true
  ): SummaryType


  def patientMatches(
    f: PatientRecord => Boolean = _ => true
  ): Seq[PatientMatch[Criteria]]


  def patientRecord(
    patId: Id[Patient]
  ): Option[PatientRecord] 

}


object ResultSet
{

  final case class Demographics
  (
    genderDistribution: Seq[ConceptCount[Coding[Gender.Value]]],
    ageDistribution: Seq[ConceptCount[Interval[Int]]],
    siteDistribution: Seq[ConceptCount[Coding[Site]]]
  )

  object Demographics extends ReportingOps
  {
    def on(patients: Seq[Patient]) =
      ResultSet.Demographics(
        distribution(patients.map(_.gender)),
        ageDistribution(patients.map(_.age)),
        distribution(patients.flatMap(_.managingSite))
      )
  }


  trait Summary
  {
    val id: Query.Id
    val patientCount: Int
    val demographics: Demographics
  }


  implicit val writesDemographics: OWrites[Demographics] =
    Json.writes[Demographics]

}
