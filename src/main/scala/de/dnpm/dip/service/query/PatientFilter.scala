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
  VitalStatus
}
import play.api.libs.json.{
  Json,
  OFormat
}


final case class PatientFilter
(
  gender: Option[Set[Coding[Gender.Value]]],
  ageRange: Option[Interval[Int]],
  vitalStatus: Option[Set[Coding[VitalStatus.Value]]]
)


object PatientFilter
{

  def on(patients: Seq[Patient]): PatientFilter = {

    val ages =
      patients
        .map(_.age)
        .map(_.value.toInt)
   
    PatientFilter(
      Some(
        patients
          .map(_.gender)
          .toSet
      ),
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
    )

  }

  def apply(
    gender: Option[Set[Coding[Gender.Value]]],
    ageMin: Option[Int],
    ageMax: Option[Int],
    vitalStatus: Option[Set[Coding[VitalStatus.Value]]]
  ): PatientFilter =
    PatientFilter(
      gender.filter(_.nonEmpty),
      Some(
        Interval(
          min = ageMin,
          max = ageMax
        )
      ),
      vitalStatus.filter(_.nonEmpty)
    )




  import scala.language.implicitConversions

  type PatientLike = {
    def gender: Coding[Gender.Value]
    def age: Age
    def vitalStatus: Coding[VitalStatus.Value]
  }


  implicit def toPredicate[P <: PatientLike](
    filter: PatientFilter
  ): P => Boolean = {
    patient =>

    import scala.language.reflectiveCalls
    import VitalStatus._

    filter.gender.fold(true)(_ contains patient.gender) &&
    filter.ageRange.fold(true)(_ contains patient.age.value.toInt) &&
    filter.vitalStatus.fold(true)(_ contains patient.vitalStatus)
  }


  implicit val format: OFormat[PatientFilter] =
    Json.format[PatientFilter]

}
