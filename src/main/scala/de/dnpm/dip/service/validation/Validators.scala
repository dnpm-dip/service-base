package de.dnpm.dip.service.validation


import scala.util.chaining._
import cats.Applicative
import cats.data.ValidatedNel
import cats.syntax.validated._
import de.ekut.tbi.validation.{
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
  Id,
  Observation,
  Patient,
  Reference
}
import Issue.{
  Error,
  Fatal,
  Path
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


    
  // for implicit conversions to NegatableValidator[Issue.Builder,T]
  private implicit lazy val defaultIssueBuild: String => Issue.Builder =
    Error(_)



  implicit def csCodingIssueValidator[T](
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
          cs => csCodingIssueValidator(cs).apply(coding)
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

}
