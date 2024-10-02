package de.dnpm.dip.service.validation


import scala.util.chaining._
import cats.Applicative
import cats.data.ValidatedNel
import cats.syntax.validated._
import de.ekut.tbi.validation.{
  CanContain,
  NegatableValidator,
  Validator
}
import de.ekut.tbi.validation.dsl._
import de.dnpm.dip.coding.{
  Coding,
  CodeSystem,
  CodeSystemProvider
}
import de.dnpm.dip.coding.hgvs.HGVS
import de.dnpm.dip.coding.UnregisteredMedication
import de.dnpm.dip.model.{
  Diagnosis,
  History,
  Id,
  Interval,
  MedicationTherapy,
  Obs,
  Observation,
  Patient,
  Procedure,
  Recommendation,
  Reference,
  Therapy,
  TherapyRecommendation
}
import Issue.{
  Error,
  Fatal,
  Info,
  Path,
  Warning
}
import shapeless.{
  Coproduct,
  :+:,
  CNil
}
import Path.syntax._


trait Validators
{


  implicit class IssueBuilderValidatedNelExtensions[T](
    val validated: ValidatedNel[Issue.Builder,T]
  ){
    def at(path: Path): ValidatedNel[Issue,T] =
      validated.leftMap(_.map(_ at path))

    def at(node: String): ValidatedNel[Issue,T] =
      validated.leftMap(_.map(_ at Path.root/node))

  }


  implicit class IssueValidatedNelExtensions[T](
    val validated: ValidatedNel[Issue,T]
  ){
    def on(path: Path): ValidatedNel[Issue,T] =
      validated.leftMap(_.map(issue => issue.copy(path = path /: issue.path)))

    def on(node: String): ValidatedNel[Issue,T] =
      validated.leftMap(_.map(issue => issue.copy(path = node /: issue.path)))

    def on(t: T)(implicit hasId: HasId[T], node: Path.Node[T]): ValidatedNel[Issue,T] =
      validated.leftMap(_.map(issue => issue.copy(path = t /: issue.path)))
  }


  // CanContain implementation for Interval sub-types
  implicit def intervalCanContain[T, C[x] <: Interval[x]]: CanContain[T,C[T]] =
    new CanContain[T,C[T]]{

      override def contains(c: C[T])(t: T): Boolean =
        c contains t

      override def containsOnly(c: C[T])(t: T): Boolean =
        c contains t
    }



  implicit val patientNode: Path.Node[Patient] =
    Path.Node("Patient")

  implicit def diagnosisNode[D <: Diagnosis]: Path.Node[D] =
    Path.Node("Diagnose")

  implicit def therapyRecommendationNode[R <: TherapyRecommendation]: Path.Node[R] =
    Path.Node("Therapie-Empfehlung")

  implicit def medTherapyNode[T <: MedicationTherapy[_]]: Path.Node[T] =
    Path.Node("Systemische-Therapie")

  implicit def procedureNode[P <: Procedure[_]]: Path.Node[P] =
    Path.Node("Prozedur")


  def MissingValue(
    name: String,
    severity: Issue.Severity.Value = Issue.Severity.Warning
  ): Issue =
    Issue(
      severity,
      s"Fehlende Angabe '$name'",
      Path.root/name
    )

  def MissingOptValue(
    name: String
  ): Issue =
    Issue(
      Issue.Severity.Info,
      s"Fehlende optionale Angabe '$name', ggf. nachprüfen, ob nachzureichen",
      Path.root/name
    )

  def MissingResult(
    name: String,
    severity: Issue.Severity.Value = Issue.Severity.Warning
  ): Issue = 
    Issue(
      severity,
      s"Fehlende(r) Befund(e) '$name'",
      Path.root/name
    )

  def MissingResult[T: Path.Node]: Issue =
    MissingResult(Path.Node[T].name)



  // For implicit conversions to NegatableValidator[Issue.Builder,T]
  private implicit lazy val defaultIssueBuilder: String => Issue.Builder =
    Error(_)


  // Unregistered Medications are valid by default,
  // as they cannot be validated against a given CodeSystem
  implicit val unregisteredMedicationValidator: NegatableValidator[Issue.Builder,Coding[UnregisteredMedication]] =
    _.validNel[Issue.Builder]


  implicit def csCodingValidator[T](
    implicit cs: CodeSystem[T]
  ): NegatableValidator[Issue.Builder,Coding[T]] =
    coding =>
      cs.concept(coding.code) must be (defined) otherwise (
        Error(s"Ungültiger Code '${coding.code.value}' gemäß Kodier-System '${cs.name}'${cs.version.map(v => s" (Version '$v')").getOrElse("")}")
      ) map (_ => coding)


  implicit def cspCodingValidator[T](
    implicit csp: CodeSystemProvider[T,cats.Id,Applicative[cats.Id]]
  ): NegatableValidator[Issue.Builder,Coding[T]] =
    coding => 
      coding.version
        .flatMap(csp.get)
        .getOrElse(csp.latest)
        .pipe {
          cs => csCodingValidator(cs)(coding)
        }


  implicit def coproductCodingValidator[H, T <: Coproduct](
    implicit
    cs: Coding.System[H],
    hVal: NegatableValidator[Issue.Builder,Coding[H]],
    tVal: NegatableValidator[Issue.Builder,Coding[T]]
  ): NegatableValidator[Issue.Builder,Coding[H :+: T]] =
    coding =>
      (
        if (coding.system == cs.uri)
          hVal(coding.asInstanceOf[Coding[H]])
        else
          tVal(coding.asInstanceOf[Coding[T]])
      )
      .map(_.asInstanceOf[Coding[H :+: T]])
      

  
  implicit def terminalCoproductCodingValidator[H](
    implicit
    hVal: NegatableValidator[Issue.Builder,Coding[H]],
  ): NegatableValidator[Issue.Builder,Coding[H :+: CNil]] =
    coding =>
      hVal(coding.asInstanceOf[Coding[H]])
        .map(_.asInstanceOf[Coding[H :+: CNil]])


  implicit val proteinChangeValidator: Validator[Issue,Coding[HGVS.Protein]] =
    coding =>
      coding.code.value must (matchRegex (HGVS.Protein.threeLetterCode) or contain ("?")) otherwise (
        Error(s"Ungültiger Code '${coding.code}', erwarte 3-Buchstaben-Format") at "Amino-Säure-Austausch"
      ) map (_ => coding)

      
  implicit def referenceValidator[T <: { def id: Id[_] }](
    implicit
    ts: Iterable[T],
    node: Path.Node[T],
  ): NegatableValidator[Issue.Builder,Reference[T]] =
    ref => ref.resolve must be (defined) otherwise (
      Fatal(s"Nicht auflösbare Referenz-ID '${ref.id.getOrElse("N/A")}' auf Objekt '${node.name}'")
    ) map (_ => ref)


  implicit def referenceValidatorTo[T <: { def id: Id[_] }](
    implicit
    t: T,
    node: Path.Node[T],
  ): NegatableValidator[Issue.Builder,Reference[T]] =
    ref => ref.resolveOn(List(t)) must be (defined) otherwise (
      Fatal(s"Nicht auflösbare Referenz-ID '${ref.id.getOrElse("N/A")}' auf Objekt '${node.name}'")
    ) map (_ => ref)


  implicit val patientValidator: Validator[Issue,Patient] =
    patient =>
      (
        patient.healthInsurance must be (defined) otherwise (MissingValue("Krankenkasse")),
        patient.dateOfDeath must be (defined) otherwise (MissingOptValue("Todesdatum"))
      )
      .errorsOr(patient) on patient


  def TherapyValidator[
    T <: Therapy: HasId: Path.Node,
  ](
    implicit
    patient: Patient,
    diagnoses: Iterable[Diagnosis],
    recommendations: Iterable[TherapyRecommendation]
  ): Validator[Issue,T] =
    therapy =>
      (
        validate(therapy.patient) at "Patient",
        validateOpt(therapy.indication) at "Indikation",
        therapy.therapyLine must be (defined) otherwise (MissingValue("Therapie-Linie")),
        therapy.period must be (defined) otherwise (MissingValue("Zeitraum")),
        validateOpt(therapy.basedOn) at "Therapie-Empfehlung",
      )
      .errorsOr(therapy) on therapy


  def RangeValidator[T](range: Interval[T]): Validator[Issue.Builder,T] =
    t => t must be (in (range)) otherwise (
      Error(s"Ungültiger Wert $t, nicht in Referenz-Bereich $range")
    ) 


  def ObservationValidator[O <: Obs: HasId: Path.Node](
    valueValidator: Validator[Issue.Builder,O#ValueType]
  )(
    implicit patient: Patient
  ): Validator[Issue,O] =
    obs =>
      (
        validate(obs.patient) at "Patient",
        valueValidator(obs.value) at "Wert",
      )
      .errorsOr(obs) on obs


  def ObservationValidator[O <: Obs: HasId: Path.Node](
    referenceRange: Interval[O#ValueType]
  )(
    implicit patient: Patient
  ): Validator[Issue,O] =
    ObservationValidator[O](RangeValidator(referenceRange))


  def ObservationValidator[O <: Obs: HasId: Path.Node](
    implicit
    patient: Patient,
    valueValidator: Validator[Issue.Builder,O#ValueType]
  ): Validator[Issue,O] =
    ObservationValidator[O](valueValidator)

}
