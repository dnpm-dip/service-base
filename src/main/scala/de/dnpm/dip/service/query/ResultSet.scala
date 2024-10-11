package de.dnpm.dip.service.query


import de.dnpm.dip.coding.Coding
import de.dnpm.dip.model.{
  Id,
  Patient,
  Gender,
  Interval,
  Site,
  Snapshot
}
import play.api.libs.json.{
  Json,
  OWrites
}


trait ResultSet[
  PatientRecord <: { def patient: Patient },
  Criteria,
]
{

  import scala.util.chaining._
  import scala.language.reflectiveCalls

  type Filter <: Filters[PatientRecord]


  def id: Query.Id

  def results: Seq[Query.Match[PatientRecord,Criteria]]

  import scala.language.implicitConversions

  protected implicit def toPredicate[F >: Filter](filter: F): PatientRecord => Boolean


  def defaultFilter: Filter

  
  protected def snapshots(
    f: PatientRecord => Boolean
  ): Seq[Snapshot[PatientRecord]] =
    results.collect {
      case Query.Match(snp,_) if f(snp.data) => snp
    }
  

  protected def patientRecords(
    f: PatientRecord => Boolean
  ): Seq[PatientRecord] = 
    results.collect {
      case Query.Match(Snapshot(patRec,_),_) if f(patRec) => patRec
    }


  def demographics[F >: Filter](
    f: F
  ): ResultSet.Demographics = 
    ResultSet.Demographics
      .on(patientRecords(f).map(_.patient))


  def patientMatches[F >: Filter](
    f: F
  ): Seq[PatientMatch[Criteria]] = {

    val filter: PatientRecord => Boolean = f

    results.collect {
      case Query.Match(Snapshot(patRec,_),matchingCriteria) if filter(patRec) =>
        PatientMatch.of(
          patRec.patient,
          matchingCriteria
        )
     }

  }


  def patientRecord(
    patId: Id[Patient]
  ): Option[PatientRecord] =
    results
      .collectFirst {
        case Query.Match(Snapshot(patRec,_),_) if patRec.patient.id == patId => patRec
      }

}


object ResultSet
{

  final case class Demographics
  (
    patientCount: Int,
    genderDistribution: Distribution[Coding[Gender.Value]],
    ageDistribution: Distribution[Interval[Int]],
    siteDistribution: Distribution[Coding[Site]]
  )

  object Demographics extends ReportingOps
  {
    def on(patients: Seq[Patient]) =
      ResultSet.Demographics(
        patients.size,
        Distribution.of(patients.map(_.gender)),
        Distribution.of(patients.map(_.age)),
        Distribution.of(patients.flatMap(_.managingSite))
      )

    implicit val writesDemographics: OWrites[Demographics] =
      Json.writes[Demographics]
  }

}
