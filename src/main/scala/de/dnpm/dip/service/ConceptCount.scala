package de.dnpm.dip.service


import play.api.libs.json.{
  Json,
  Reads,
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

  implicit val reads: Reads[Count] =
    Json.reads[Count]

  implicit val writes: OWrites[Count] =
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


