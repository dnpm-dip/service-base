package de.dnpm.dip.service.query


import play.api.libs.json.{
  Json,
  OWrites
}


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


object ConceptCount
{

  def apply[T](
    t: T,
    count: Count,
    children: Option[Seq[ConceptCount[T]]] = None
  ): ConceptCount[T] =
    Entry(t,count,children)

}


