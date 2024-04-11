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


    import cats.Semigroup

    def combineWith(other: ValidatedNel[Issue,T]) = {

      implicit val sg: Semigroup[T] =
        Semigroup.instance((t,_) => t)

      validated combine other
    }
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
    node: Path.Node[T],
  ): NegatableValidator[Issue.Builder,Reference[T]] =
    ref =>
      ref.resolveOn(ts) must be (defined) otherwise (
        Fatal(s"Nicht auflösbare Referenz-ID '${ref.id.getOrElse("N/A")}' auf Objekt '${node.name}'")
      ) map (_ => ref)


  implicit def referenceValidatorTo[T: HasId](
    implicit
    t: T,
    node: Path.Node[T],
  ): NegatableValidator[Issue.Builder,Reference[T]] =
    ref =>
      ref.resolveOn(List(t)) must be (defined) otherwise (
        Fatal(s"Nicht auflösbare Referenz-ID '${ref.id.getOrElse("N/A")}' auf Objekt '${node.name}'")
      ) map (_ => ref)


  implicit def therapyValidator[
    T <: Therapy: HasId: Path.Node,
  ](
    implicit
    patient: Patient,
    diagnoses: Iterable[Diagnosis],
    recommendations: Iterable[TherapyRecommendation]
  ): Validator[Issue,T] = {
    therapy =>
      (
        validate(therapy.patient) at "Patient",
        validate(therapy.indication) at "Indikation",
        therapy.therapyLine must be (defined) otherwise (
          Warning("Fehlende Angabe") at "Therapie-Linie"
        ),
        therapy.period must be (defined) otherwise (
          Warning("Fehlende Angabe") at "Zeitraum"
        ),
        ifDefined (therapy.basedOn)(validate(_)) at "Therapie-Empfehlung",
      )
      .errorsOr(therapy) on therapy
  }



  def ObservationValidator[V,Obs <: Observation[V]: HasId: Path.Node](
    valueValidator: Validator[Issue.Builder,V]
  )(
    implicit
    patient: Patient,
  ): Validator[Issue,Obs] =
    obs =>
      (
        validate(obs.patient) at "Patient",
        valueValidator(obs.value) at "Wert",
      )
      .errorsOr(obs) on obs


  implicit def ObsValidator[V, Obs <: Observation[V]: HasId: Path.Node](
    implicit
    patient: Patient,
    valueValidator: Validator[Issue.Builder,V]
  ): Validator[Issue,Obs] =
    ObservationValidator[V,Obs](valueValidator)

}
