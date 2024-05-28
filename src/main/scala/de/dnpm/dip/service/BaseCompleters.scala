package de.dnpm.dip.service


import cats.{
  Applicative,
  Id
}
import de.dnpm.dip.util.Completer
import de.dnpm.dip.coding.{
  Code,
  Coding,
  CodeSystem,
  CodeSystemProvider,
  CodeSystemProviders
}
import shapeless.{
  Coproduct
}


trait BaseCompleters
{

  private def expandDescendantCodings[T,U >: T](
    code: Code[T],
    cs: CodeSystem[U]
  ): Set[Coding[T]] =
    (cs.concept(code).toSet ++ cs.descendantsOf(code))
      .map(
        _.toCoding(cs.uri)
         .asInstanceOf[Coding[T]]
      )


  private def expandDescendants[T,U >: T](
    coding: Coding[T],
    cs: CodeSystem[U]
  ): Set[Coding[T]] =
    (cs.concept(coding.code).toSet ++ cs.descendantsOf(coding.code))
      .map(
        _.toCoding(coding.system)
         .asInstanceOf[Coding[T]]
      )


  def expandDescendants[T,U >: T](
    coding: Coding[T],
    csp: CodeSystemProvider[U,Id,Applicative[Id]]
  ): Set[Coding[T]] =
    expandDescendants(
      coding,
      coding.version
        .flatMap(csp.get)
        .getOrElse(csp.latest)
    )


  def expandDescendantCodings[T: Coding.System](
    code: Code[T]
  )(
    implicit csp: => CodeSystemProvider[T,Id,Applicative[Id]]
  ): Set[Coding[T]] = {

    val cs = csp.latest

    (cs.concept(code).toSet ++ cs.descendantsOf(code))
      .map(_.toCoding)
  }

  def expandDescendants[T](
    coding: Coding[T]
  )(
    implicit csp: => CodeSystemProvider[T,Id,Applicative[Id]]
  ): Set[Coding[T]] =
    expandDescendants(
      coding,
      coding.version
        .flatMap(csp.get)
        .getOrElse(csp.latest)
    )


  // By-name csp parameter (i.e. "lazy" as only evaluated upon being referenced)
  // is required because in traits, the value is usually not yet initialized at this point,
  // resulting in weird null pointer exception
  def descendantExpander[T: Coding.System](
    implicit csp: => CodeSystemProvider[T,Id,Applicative[Id]]
  ): Completer[Set[Coding[T]]] =
    Completer.of(
      _.flatMap(coding => expandDescendants(coding,csp))
    )


  // By-name csps parameter (i.e. "lazy" as only evaluated upon being referenced)
  // is required because in traits, the value is usually not yet initialized at this point,
  // resulting in weird null pointer exception
  def descendantExpanderOf[CS <: Coproduct](
    implicit csps: => CodeSystemProviders[CS]
  ): Completer[Set[Coding[CS]]] =
    Completer.of(
      _.flatMap(
        coding =>
          expandDescendants(
            coding,
            csps.values(coding.system))  // Map.apply safe here, because the code won't compile
          )                              // if not all CodeSystemProviders are in scope, so csps is sure to contain all systems
    )

}
