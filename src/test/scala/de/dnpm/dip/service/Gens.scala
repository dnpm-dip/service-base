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
  CarePlan,
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
import de.dnpm.dip.service.mvh.{
  BroadConsent,
  Consent,
  ModelProjectConsent,
  Submission,
  TransferTAN
}
import play.api.libs.json.Json


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


  def genCarePlan(
    patient: Patient
  ): Gen[DummyCarePlan] =
    for { 
      id <- Gen.of[Id[DummyCarePlan]]
    } yield DummyCarePlan(
      id,
      Reference.to(patient),
      LocalDate.now,
      Some(Coding(CarePlan.NoSequencingPerformedReason.Other))
    )


  implicit val genDummyPatientRecord: Gen[DummyPatientRecord] =
    for { 
      patient <- Gen.of[Patient]

      episode <- genEpisodeOfCare(patient)

      diagnosis <- genDiagnosis(patient)

      carePlan <- genCarePlan(patient)

    } yield DummyPatientRecord(
      patient,
      NonEmptyList.of(episode),
      NonEmptyList.of(diagnosis),
      NonEmptyList.of(carePlan)
    )


  private lazy val broadConsent =
    Json.fromJson[BroadConsent](
      Json.parse(getClass.getClassLoader.getResourceAsStream("consent.json"))
    )
    .get


  def genMetadata(
    record: DummyPatientRecord,
    submissionType: Submission.Type.Value,// = Submission.Type.Initial,
    withBroadConsent: Boolean// = true
  ): Gen[Submission.Metadata] =
    for {
      ttan <- Gen.listOf(64, Gen.oneOf("0","1","2","3","4","5","6","7","8","9","A","B","C","D","E","F")).map(_.mkString)

      consentDate =
        record.getCarePlans
          .map(_.issuedOn)
          .minOption
          .map(_ minusWeeks 2)
          .getOrElse(LocalDate.now)

      (bc,noBcReason) =
        if (withBroadConsent) Some(List(broadConsent)) -> None
        else None -> Some(BroadConsent.ReasonMissing.OrganizationalIssues)

    } yield Submission.Metadata(
      submissionType,
      Id[TransferTAN](ttan),
      ModelProjectConsent(
        "Patient Info TE Consent MVGenomSeq vers01",
        Some(consentDate minusDays 1),
        ModelProjectConsent.Purpose.values
          .toList
          .map(
            Consent.Provision(
              consentDate,
              _,
              Consent.Provision.Type.Permit
            )
          )
      ),
      bc,
      noBcReason
    )

/*    
  def genMetadata(
    consentDate: LocalDate,
    submissionType: Submission.Type.Value = Submission.Type.Initial,
    withBroadConsent: Boolean = true
  ): Gen[Submission.Metadata] =
    for {
      ttan <- Gen.listOf(64, Gen.oneOf("0","1","2","3","4","5","6","7","8","9","A","B","C","D","E","F")).map(_.mkString)

      (bc,noBcReason) =
        if (withBroadConsent) Some(List(broadConsent)) -> None
        else None -> Some(BroadConsent.ReasonMissing.OrganizationalIssues)

    } yield Submission.Metadata(
      submissionType,
      Id[TransferTAN](ttan),
      ModelProjectConsent(
        "Patient Info TE Consent MVGenomSeq vers01",
        Some(consentDate minusDays 1),
        ModelProjectConsent.Purpose.values
          .toList
          .map(
            Consent.Provision(
              consentDate,
              _,
              Consent.Provision.Type.Permit
            )
          )
      ),
      bc,
      noBcReason
    )

  implicit val genDataUpload: Gen[DataUpload[DummyPatientRecord]] =
    for {
      record <- Gen.of[DummyPatientRecord]
      metadata <- genMetadata(record)
    } yield DataUpload(
      record,
      Some(metadata)
    )
*/

}
