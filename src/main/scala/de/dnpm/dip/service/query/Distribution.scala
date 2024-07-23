package de.dnpm.dip.service.query


import cats.Semigroup
import play.api.libs.json.{
  Json,
  Writes,
  OWrites
}
import de.dnpm.dip.model.{
  Age,
  Site,
  Interval,
  ClosedInterval,
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


  private def combine[T](
    c1: ConceptCount[T],
    c2: ConceptCount[T]
  )(
    implicit counter: Int => Count
  ): ConceptCount[T] =
    ConceptCount(
      c1.key,
      counter(c1.value.count + c2.value.count),
      (c1.children,c2.children) match { 
        case (Some(ch1),Some(ch2)) => Some(combineAll(ch1,ch2))
        case (Some(ch1),_)         => Some(ch1)
        case (_,Some(ch2))         => Some(ch2)
        case (_,_)                 => None

      }
    )

  private def combineAll[T](
    c1s: Seq[ConceptCount[T]],
    c2s: Seq[ConceptCount[T]]
  )(
    implicit counter: Int => Count
  ): Seq[ConceptCount[T]] = {
    (c1s ++ c2s)
      .groupMapReduce(_.key)(identity)(combine(_,_))
      .values
      .toSeq
      .sorted
  }

  implicit def semigroup[T]: Semigroup[Distribution[T]] = {

    implicit def conceptCountSemigroup(
      implicit counter: Int => Count
    ): Semigroup[ConceptCount[T]] =
      Semigroup.instance { combine(_,_) }


    Semigroup.instance { 
      (d1,d2) =>
        implicit val counter =
          Count.total(d1.total + d2.total)

        Distribution(
          d1.total + d2.total,
          combineAll(d1.elements,d2.elements)
        )
    }
  }


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

        // Get minimum age, rounded down to next multiple of step size
        val min =
          ages.min
            .pipe(_.value)
            .pipe(v => (v/step).floor.toInt * step)

        // Get maximum age, rounded up to next multiple of step size
        val max =
          ages.max
            .pipe(_.value)
            .pipe(
              v =>
                (v/step).ceil.toInt * step match {
                  case max if max == min => max + step // ensure at least 1 interval is created even in case there's only one distinct age value, i.e. min == max
                  case max => max
                }
            )

        Distribution(
          ages.size,
          LazyList
            .unfold(min)(
              l => (l + step) match {
                case r if r < max  => Some(LeftClosedRightOpenInterval(l,r),r)

                case r if r == max => Some(ClosedInterval(l,r),r)

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


  def byParentAndBy[T,U](
    ts: Seq[T],
  )(
    parent: T => T,
    f: T => U
  ): Distribution[U] = {

    def subCounts(
      ts: Seq[T],
      counter: Int => Count
    ): Seq[ConceptCount[U]] =
      ts.groupBy(f)
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
      ts.groupBy(parent andThen f)
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

  def byParent[T](
    ts: Seq[T],
    parent: T => T,
  ): Distribution[T] =
    byParentAndBy(ts)(parent,identity)



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
