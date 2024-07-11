package de.dnpm.dip.service.query


import java.time.LocalDate.{now => today}
import java.time.temporal.ChronoUnit.YEARS 
import de.dnpm.dip.coding.Coding
import de.dnpm.dip.model.{
  Age,
  Interval,
  ClosedInterval,
  Gender,
  Patient,
  VitalStatus,
  Site
}
import play.api.libs.json.{
  Json,
  OWrites
}


final case class PatientFilter
(
  gender: Option[Set[Coding[Gender.Value]]],
  ageRange: Option[Interval[Int]],
  vitalStatus: Option[Set[Coding[VitalStatus.Value]]],
  site: Option[Set[Coding[Site]]]
)
extends (Patient => Boolean)
{
  override def apply(patient: Patient): Boolean = {

    import VitalStatus._

    gender.fold(true)(_ contains patient.gender) &&
    ageRange.fold(true)(_ contains patient.age.value.toInt) &&
    vitalStatus.fold(true)(_ contains patient.vitalStatus) &&
    site.fold(true)(sites => patient.managingSite exists (c => sites exists (_.code == c.code)))
  }

}


object PatientFilter
{

  def from(patients: Seq[Patient]): PatientFilter = {

    val ages =
      patients
        .map(_.age)
        .map(_.value.toInt)
   
    PatientFilter(
      Some(
        patients
          .map(_.gender)
          .toSet
      )
      .filter(_.nonEmpty),
      Some(
        ClosedInterval(
          ages.minOption.getOrElse(0) -> ages.maxOption.getOrElse(0)
        )
      ),
      Some(
        patients
        .map(_.vitalStatus)
        .toSet
      )
      .filter(_.nonEmpty),
      Some(
        patients.flatMap(
          _.managingSite
        )
        .toSet
      )
      .filter(_.nonEmpty)
    )

  }

  def on[PatientRecord <: { def patient: Patient }](
    records: Seq[PatientRecord]
  ): PatientFilter = {

    import scala.language.reflectiveCalls

    from(records.map(_.patient))
  }


  def apply(
    gender: Option[Set[Coding[Gender.Value]]],
    ageMin: Option[Int],
    ageMax: Option[Int],
    vitalStatus: Option[Set[Coding[VitalStatus.Value]]],
    site: Option[Set[Coding[Site]]]
  ): PatientFilter =
    PatientFilter(
      gender.filter(_.nonEmpty),
      Some(
        Interval(
          min = ageMin,
          max = ageMax
        )
      ),
      vitalStatus.filter(_.nonEmpty),
      site.filter(_.nonEmpty)
    )


  lazy val empty: PatientFilter =
    PatientFilter(None,None,None,None)


  implicit val format: OWrites[PatientFilter] =
    Json.writes[PatientFilter]

}
