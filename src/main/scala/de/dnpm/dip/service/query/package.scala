package de.dnpm.dip.service

import play.api.libs.json.{
  Json,
  OWrites
}

package object query
{


final case class Count
(
  count: Int,
  percent: Double
)

object Count 
{

  def of(n: Int, total: Int): Count =
    Count(n,(n.toDouble/total)*100)

  def total(total: Int): Int => Count =
    Count.of(_,total)


  implicit val ordering: Ordering[Count] =
    Ordering[Int]
      .on[Count](_.count)
      .reverse


  implicit val formatCount: OWrites[Count] =
    Json.writes[Count]
}


type ConceptCount[+T] = Entry[T,Count]

/*
implicit def conceptCountOrdering[T]: Ordering[ConceptCount[T]] =
  Ordering[Int]
    .on[ConceptCount[T]](_.value.count)
    .reverse
*/


object ConceptCount
{

  def apply[T](
    t: T,
    count: Count,
    children: Option[Seq[ConceptCount[T]]] = None
  ): ConceptCount[T] =
    Entry(t,count,children)

}


type DistributionsBy[K,T] = Seq[Entry[K,Distribution[T]]]

}
