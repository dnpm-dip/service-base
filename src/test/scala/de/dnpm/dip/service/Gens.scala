package de.dnpm.dip.service


import java.time.{
  LocalDate,
  YearMonth
}
import java.time.temporal.ChronoUnit.YEARS
import cats.data.NonEmptyList
import de.ekut.tbi.generators.Gen
import de.ekut.tbi.generators.DateTimeGens._
import de.dnpm.dip.coding.Coding
import de.dnpm.dip.model.{
  Address,
  Diagnosis,
  EpisodeOfCare,
  ExternalId,
  Period,
  Reference,
  Gender,
  HealthInsurance,
  IK,
  Id,
  Patient
}


object Gens
{

  implicit def genId[T]: Gen[Id[T]] =
    Gen.uuidStrings.map(Id(_))

  private val genGender: Gen[Coding[Gender.Value]] =
    Gen.distribution(
      49.0 -> Gender.Male,
      49.0 -> Gender.Female,
      2.0  -> Gender.Other,
    )
    .map(Coding(_))


  implicit val genPatient: Gen[Patient] =
    for {
      id <- Gen.of[Id[Patient]]

      gender <- genGender

      birthDate <-
        localDatesBetween(
          LocalDate.now.minusYears(70),
          LocalDate.now.minusYears(30)
        )

      age = YEARS.between(birthDate,LocalDate.now)

      dateOfDeath <-
        Gen.option(
          Gen.longsBetween(age - 20L, age - 5L).map(birthDate.plusYears),
          0.4
        )

      healthInsurance =
        Patient.Insurance(
          Coding(HealthInsurance.Type.GKV),
          Some(
            Reference(ExternalId[HealthInsurance,IK]("1234567890"))
              .withDisplay("AOK")
            )
        )

    } yield Patient(
      id,
      gender,
      YearMonth.from(birthDate),
      dateOfDeath.map(YearMonth.from),
      None,
      healthInsurance,
      Some(Address(Address.MunicipalityCode("12345")))
    )


  def genDiagnosis(
    patient: Patient
  ): Gen[DummyDiagnosis] =
    for { 
      id <- Gen.of[Id[Diagnosis]]
    } yield DummyDiagnosis(
      id,
      Reference.to(patient),
      LocalDate.now
    )


  def genEpisodeOfCare(
    patient: Patient
  ): Gen[DummyEpisodeOfCare] =
    for { 
      id <- Gen.of[Id[EpisodeOfCare]]
      period = Period(LocalDate.now.minusMonths(6))
    } yield DummyEpisodeOfCare(
      id,
      Reference.to(patient),
      period
    )


  implicit val genDummyPatientRecord: Gen[DummyPatientRecord] =
    for { 
      patient <- Gen.of[Patient]

      episode <- genEpisodeOfCare(patient)

      diagnosis <- genDiagnosis(patient)

    } yield DummyPatientRecord(
      patient,
      NonEmptyList.of(episode),
      NonEmptyList.of(diagnosis)
    )

}
