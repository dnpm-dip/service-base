package de.dnpm.dip.service.validation


import cats.{
  Applicative,
}
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
  Reference
}
import ValidationReport.Issue
import Issue.{
  Error,
  Location
}



trait Validators
{

  def validCoding[T](
    implicit cs: CodeSystem[T]
  ): NegatableValidator[String,Coding[T]] =
    coding =>
      cs.concept(coding.code) must be (defined) otherwise (
       s"Invalid code '${coding.code.value}'"
      ) map (_ => coding)

  def validCoding[T](
    implicit csp: CodeSystemProvider[T,cats.Id,Applicative[cats.Id]]
  ): NegatableValidator[String,Coding[T]] =
    coding => 
      coding.version
        .flatMap(csp.get)
        .getOrElse(csp.latest)
        .concept(coding.code) must be (defined) otherwise (
          s"Invalid code '${coding.code.value}'"
        ) map (_ => coding)


  def validCodingAt[T](
    location: => Location
  )(
    implicit cs: CodeSystem[T]
  ): NegatableValidator[Issue,Coding[T]] = {

    implicit lazy val errorAt: String => Issue =
      Error(_) at location

    _ must be (validCoding(cs)) leftMap (_ map errorAt)
  }

  def validCodingAt[T](
    location: => Location
  )(
    implicit csp: CodeSystemProvider[T,cats.Id,Applicative[cats.Id]]
  ): NegatableValidator[Issue,Coding[T]] = {

    implicit lazy val errorAt: String => Issue =
      Error(_) at location

    _ must be (validCoding[T](csp)) leftMap (_ map errorAt)
  }

/*
  def validCoding[T](
    location: => Location
  )(
    implicit cs: CodeSystem[T]
  ): Validator[Issue,Coding[T]] =
    coding =>
      cs.concept(coding.code) must be (defined) otherwise (
        Error(s"Invalid code '${coding.code.value}'") at location
      ) map (_ => coding)


  def validCoding[T](
    location: => Location
  )(
    implicit csp: CodeSystemProvider[T,cats.Id,Applicative[cats.Id]]
  ): Validator[Issue,Coding[T]] =
    coding => 
      coding.version
        .flatMap(csp.get)
        .getOrElse(csp.latest)
        .concept(coding.code) must be (defined) otherwise (
          Error(s"Invalid code '${coding.code.value}'") at location
        ) map (_ => coding)
*/


  def validReference[T](
    implicit
    hadId: T <:< { def id: Id[T] },
    ts: Iterable[T]
  ): NegatableValidator[String,Reference[T]] =
    ref =>
      ref.resolveOn(ts) must be (defined) otherwise (
        s"Invalid reference ID ${ref.id.getOrElse("N/A")}"
      ) map (_ => ref)


  def validReferenceAt[T](
    location: => Location
  )(
    implicit
    hadId: T <:< { def id: Id[T] },
    ts: Iterable[T]
  ): NegatableValidator[Issue,Reference[T]] = {

    implicit lazy val errorAt: String => Issue =
      Issue.Fatal(_) at location

    _ must be (validReference[T]) leftMap (_ map errorAt)

  }

}
