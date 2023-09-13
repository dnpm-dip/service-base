package de.dnpm.dip.service.query


import play.api.libs.json._
import cats.Semigroup


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
  implicit def conceptCountOrdering[T]: Ordering[ConceptCount[T]] =
    Ordering[Int]
      .on[ConceptCount[T]](_.count)
      .reverse


  implicit def conceptCountSemigroup[T]: Semigroup[ConceptCount[T]] =
    new Semigroup[ConceptCount[T]]{
      self =>

      override def combine(cc1: ConceptCount[T], cc2: ConceptCount[T]) = {

        if (cc1.concept == cc2.concept){
          ConceptCount(
            cc1.concept,
            cc1.count + cc2.count,
            (cc1.components,cc2.components) match {
              case (Some(comps1),Some(comps2)) =>
                Some(
                  (comps1 ++ comps2)
                    .groupMapReduce(_.concept)(identity)(self.combine(_,_))
                    .values
                    .toSeq
                    .sorted
                )
              case (Some(comps1),None) => Some(comps1)
              case (None,Some(comps2)) => Some(comps2)
              case (None,None)         => None

            }
          )
        }
        else
          cc1
      }
    }


}
