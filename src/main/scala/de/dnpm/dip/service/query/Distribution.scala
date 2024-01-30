package de.dnpm.dip.service.query


import play.api.libs.json.{
  Json,
  Writes,
  OWrites
}
import de.dnpm.dip.model.{
  Age,
  Site,
  Interval,
  LeftClosedRightOpenInterval
}



final case class Distribution[T]
(
  total: Int,
  elements: Seq[ConceptCount[T]]
)


object Distribution
{

  import scala.util.chaining._


  def by[T,U](
    ts: Seq[T]
  )(
    grp: T => U
  ): Distribution[U] = {

    val counter =
      Count.total(ts.size)

    Distribution(
      ts.size,
      ts.groupBy(grp)
        .map {
          case (u,seq) =>
            ConceptCount(
              u,
              counter(seq.size),
              None
            )
        }
        .toSeq
        .sorted
    )

  }


  def of[T](
    ts: Seq[T]
  ): Distribution[T] =
    by(ts)(identity)



  def ofAge(
    values: Seq[Age],
    step: Int = 5
  ): Distribution[Interval[Int]] = {

    import Interval._  // for syntax "x isIn interval"

    values match {
      case ages if ages.nonEmpty =>

        val counter =
          Count.total(ages.size)

        // Get minimum age, rounded down to multiple of step size
        val min =
          ages.min
            .pipe(_.value.toInt)
            .pipe(n => n - (n % step))

        // Get maximum age, rounded up to multiple of step size
        val max =
          ages.max
            .pipe(_.value.toInt)
            .pipe(n => n + (n % step))

        Distribution(
          ages.size,
          LazyList
            .unfold(min)(
              l => (l + step) match {
                case r if r <= max => Some(LeftClosedRightOpenInterval(l,r),r)
                case _             => None
              }
            )
            .map(
              range =>
                ConceptCount(
                  range,
                  counter(ages count (_.value.toInt isIn range))
                )
            )
        )

      case _ =>
        Distribution(
          0,
          Seq.empty
        )
    }

  }


  def binned[T](
    values: Seq[T],
    binSize: T,
  )(
    implicit num: Numeric[T]
  ): Distribution[Interval[T]] = {

    values match {

      case ts if ts.nonEmpty =>

        val counter =
          Count.total(ts.size)

        val min = ts.min

        val max = ts.max

        Distribution(
          ts.size,
          LazyList
            .unfold(min)(
              s => num.plus(s,binSize) match {
                case t if num.lteq(t,max) => Some(LeftClosedRightOpenInterval(s,t),t)
                case _ => None
              }
            )
            .map(
              range =>
                ConceptCount(
                  range,
                  counter(ts count (range.contains))
                )
            )
        )

      case _ =>
        Distribution(
          0,
          Seq.empty
        )

    }

  }


  def byParent[T](
    ts: Seq[T],
    parent: T => T
  ): Distribution[T] = {

    def subCounts[T](
      ts: Seq[T],
      counter: Int => Count
    ): Seq[ConceptCount[T]] =
      ts.groupBy(identity)
        .map {
          case (t,seq) =>
            ConceptCount(
              t,
              counter(seq.size),
              None
            )
        }
        .toSeq
        .sorted


    val counter =
      Count.total(ts.size)

    Distribution(
      ts.size,
      ts.groupBy(parent)
        .map {
          case (p,children) =>
            ConceptCount(
              p,
              counter(children.size),
              Some(subCounts(children,counter))
            )
        }
        .toSeq
        .sorted
    )

  }


  def associatedOn[A,C,T](
    records: Seq[A]
  )(
    csOn: A => Seq[C],
    tsOn: A => Seq[T]
  ): DistributionsBy[C,T] =
    records.foldLeft(
      Map.empty[C,Seq[T]]
    ){
      (acc,record) =>

      val cs =
        csOn(record)

      val ts =
        tsOn(record)

      cs.foldLeft(acc){
        (accPr,c) =>
          accPr.updatedWith(c)(
            _.map(_ :++ ts)
             .orElse(Some(ts))
          )
      }

    }
    .map {
      case (c,ts) =>
        Entry(
          c,
          Distribution.of(ts)
        )
    }
    .toSeq


  implicit def writes[T: Writes]: OWrites[Distribution[T]] =
    Json.writes[Distribution[T]]

}
