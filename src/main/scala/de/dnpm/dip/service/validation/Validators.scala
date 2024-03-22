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

  implicit class IssueBuilderValidatedNelExtensions[T](
    val v: ValidatedNel[Issue.Builder,T]
  ){
    def at(path: Path): ValidatedNel[Issue,T] =
      v.leftMap(_.map(_ at path))
  }


  implicit def csCodingIssueValidator[T](
    implicit cs: CodeSystem[T]
  ): NegatableValidator[Issue.Builder,Coding[T]] = {

    // Required in scope for conversion into NegatableValidator
    implicit lazy val err: String => Issue.Builder =
      Error(_)

    coding =>
      cs.concept(coding.code) must be (defined) otherwise (
        Error(s"Ungültiger Code '${coding.code.value}' gemäß Kodier-System '${cs.name}'${cs.version.map(v => s" (Version '$v')").getOrElse("")}")
      ) map (_ => coding)
  }

  implicit def cspCodingValidator[T](
    implicit csp: CodeSystemProvider[T,cats.Id,Applicative[cats.Id]]
  ): NegatableValidator[Issue.Builder,Coding[T]] = {

    // Required in scope for conversion into NegatableValidator
    implicit lazy val err: String => Issue.Builder =
      Error(_)

    coding => 
      coding.version
        .flatMap(csp.get)
        .getOrElse(csp.latest)
        .pipe {
          cs => csCodingIssueValidator(cs).apply(coding)
        }
  }

  implicit def coproductCodingValidator[H, T <: Coproduct](
    implicit
    csp: CodeSystemProvider[H,cats.Id,Applicative[cats.Id]],
    tVal: NegatableValidator[Issue.Builder,Coding[T]]
  ): NegatableValidator[Issue.Builder,Coding[H :+: T]] = {

    // Required in scope for conversion into NegatableValidator
    implicit lazy val err: String => Issue.Builder =
      Error(_)

    coding =>
      (
        if (coding.system == csp.uri)
          validate(coding.asInstanceOf[Coding[H]])
        else
          tVal(coding.asInstanceOf[Coding[T]])
      )
      .map(_.asInstanceOf[Coding[H :+: T]])
      
  }
  
  implicit def terminalCoproductCodingValidator[H](
    implicit
    csp: CodeSystemProvider[H,cats.Id,Applicative[cats.Id]]
  ): NegatableValidator[Issue.Builder,Coding[H :+: CNil]] = {

    // Required in scope for conversion into NegatableValidator
    implicit lazy val err: String => Issue.Builder =
      Error(_)

    coding => validate(coding.asInstanceOf[Coding[H]]) map (_.asInstanceOf[Coding[H :+: CNil]])
  }



  implicit def referenceValidator[T](
    implicit
    hasId: T <:< { def id: Id[T] },
    ts: Iterable[T],
    node: Path.Node[T]
  ): NegatableValidator[Issue.Builder,Reference[T]] = {

    // Required in scope for conversion into NegatableValidator
    implicit lazy val err: String => Issue.Builder =
      Error(_)

    ref =>
      ref.resolveOn(ts) must be (defined) otherwise (
        Fatal(s"Nicht auflösbare Referenz-ID '${ref.id.getOrElse("N/A")}' auf Objekt '${node.name}'")
      ) map (_ => ref)
  }

  implicit def referenceValidatorTo[T](
    implicit
    hasId: T <:< { def id: Id[T] },
    t: T,
    node: Path.Node[T]
  ): NegatableValidator[Issue.Builder,Reference[T]] = {

    // Required in scope for conversion into NegatableValidator
    implicit lazy val err: String => Issue.Builder =
      Error(_)

    ref =>
      ref.resolveOn(List(t)) must be (defined) otherwise (
        Fatal(s"Nicht auflösbare Referenz-ID '${ref.id.getOrElse("N/A")}' auf Objekt '${node.name}'")
      ) map (_ => ref)
  }

}
