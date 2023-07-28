package de.dnpm.dip.service.query


import play.api.libs.json._



final case class ConceptCount[T]
(
  concept: T,
  count: Int,
  components: Option[Seq[ConceptCount[T]]]
)

object ConceptCount
{
  import play.api.libs.functional.syntax._

  // Explicit Format definition required because of recursive type ConceptCount
  implicit def format[T: Format]: Format[ConceptCount[T]] =
    (
      (JsPath \ "concept").format[T] and
      (JsPath \ "count").format[Int] and
      (JsPath \ "components").lazyFormatNullable(Format.of[Seq[ConceptCount[T]]]) // lazyFormat to handle recursivity
    )(
      ConceptCount.apply _,
      unlift(ConceptCount.unapply)
    )

  // Order ConceptCounts by decreasing number of occurrence
  implicit def conceptCountOrdering[T] =
    Ordering[Int]
      .on[ConceptCount[T]](_.count)
      .reverse

}
