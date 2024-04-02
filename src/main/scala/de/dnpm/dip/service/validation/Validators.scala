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
import de.dnpm.dip.model.{
  Diagnosis,
  Id,
  Interval,
  MedicationTherapy,
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
  Path,
  Warning
}
import shapeless.{
  Coproduct,
  :+:,
  CNil
}


trait Validators
{

  private type HasId[T] = T <:< { def id: Id[T] }


  implicit class IssueBuilderValidatedNelExtensions[T](
    val v: ValidatedNel[Issue.Builder,T]
  ){
    def at(path: Path): ValidatedNel[Issue,T] =
      v.leftMap(_.map(_ at path))
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


    
  // For implicit conversions to NegatableValidator[Issue.Builder,T]
  private implicit lazy val defaultIssueBuilder: String => Issue.Builder =
    Error(_)


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
    csp: CodeSystemProvider[H,cats.Id,Applicative[cats.Id]],
    tVal: NegatableValidator[Issue.Builder,Coding[T]]
  ): NegatableValidator[Issue.Builder,Coding[H :+: T]] =
    coding =>
      (
        if (coding.system == csp.uri)
          validate(coding.asInstanceOf[Coding[H]])
        else
          tVal(coding.asInstanceOf[Coding[T]])
      )
      .map(_.asInstanceOf[Coding[H :+: T]])
      

  
  implicit def terminalCoproductCodingValidator[H](
    implicit
    csp: CodeSystemProvider[H,cats.Id,Applicative[cats.Id]]
  ): NegatableValidator[Issue.Builder,Coding[H :+: CNil]] =
    coding =>
      validate(coding.asInstanceOf[Coding[H]]) map (_.asInstanceOf[Coding[H :+: CNil]])


  implicit def referenceValidator[T: HasId](
    implicit
    ts: Iterable[T],
    node: Path.Node[T]
  ): NegatableValidator[Issue.Builder,Reference[T]] =
    ref =>
      ref.resolveOn(ts) must be (defined) otherwise (
        Fatal(s"Nicht auflösbare Referenz-ID '${ref.id.getOrElse("N/A")}' auf Objekt '${node.name}'")
      ) map (_ => ref)


  implicit def referenceValidatorTo[T: HasId](
    implicit
    t: T,
    node: Path.Node[T]
  ): NegatableValidator[Issue.Builder,Reference[T]] =
    ref =>
      ref.resolveOn(List(t)) must be (defined) otherwise (
        Fatal(s"Nicht auflösbare Referenz-ID '${ref.id.getOrElse("N/A")}' auf Objekt '${node.name}'")
      ) map (_ => ref)


  implicit def baseTherapyValidator[T <: Therapy: HasId: Path.Node](
    implicit
    basePath: Path,
    patient: Patient,
    diagnoses: Iterable[Diagnosis],
  ): Validator[Issue,T] = {
    therapy =>
      val path = basePath/therapy
      (
        validate(therapy.patient) at path/"Patient",
        validate(therapy.indication) at path/"Indikation",
        therapy.therapyLine must be (defined) otherwise (
          Warning("Fehlende Angabe") at path/"Therapie-Linie"
        ),
        therapy.period must be (defined) otherwise (
          Warning("Fehlende Angabe") at path/"Zeitraum"
        ),
      )
      .errorsOr(therapy)
  }


/*
  implicit def therapyValidator[
    T <: Therapy: HasId: Path.Node,
    D <: Diagnosis: HasId,
    R <: TherapyRecommendation: HasId
  ](
    implicit
    basePath: Path,
    patient: Patient,
    diagnoses: Iterable[D],
    recommendations: Iterable[R]
  ): Validator[Issue,T] = {
    therapy =>
      val path = basePath/therapy
      (
        validate(therapy.patient) at path/"Patient",
        validate(therapy.indication) at path/"Indikation",
        therapy.therapyLine must be (defined) otherwise (
          Warning("Fehlende Angabe") at path/"Therapie-Linie"
        ),
        therapy.period must be (defined) otherwise (
          Warning("Fehlende Angabe") at path/"Zeitraum"
        ),
        ifDefined (therapy.basedOn)(validate(_)) at path/"Therapie-Empfehlung",
      )
      .errorsOr(therapy)
  }
*/


  def ObservationValidator[V,Obs <: Observation[V]: HasId: Path.Node](
    basePath: Path,
    valueValidator: Validator[Issue.Builder,V]
  )(
    implicit
    patient: Patient,
  ): Validator[Issue,Obs] = {
    obs =>
      val path = basePath/obs

      (
        validate(obs.patient) at path/"Patient",
        valueValidator(obs.value) at path/"Wert",
      )
      .errorsOr(obs)
  }

  implicit def ObsValidator[V, Obs <: Observation[V]: HasId: Path.Node](
    implicit
    basePath: Path,
    patient: Patient,
    valueValidator: Validator[Issue.Builder,V]
  ): Validator[Issue,Obs] =
    ObservationValidator[V,Obs](basePath,valueValidator)


/*
  implicit def ObsValidator[V, Obs <: Observation[V]: HasId: Path.Node](
    implicit
    basePath: Path,
    patient: Patient,
    valueValidator: Validator[Issue.Builder,V]
  ): Validator[Issue,Obs] = {
    obs =>
      val path = basePath/obs

      (
        validate(obs.patient) at path/"Patient",
        validate(obs.value) at path/"Wert",
      )
      .errorsOr(obs)
  }
*/

}
