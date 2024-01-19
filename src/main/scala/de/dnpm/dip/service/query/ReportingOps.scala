package de.dnpm.dip.service.query


import java.time.LocalDateTime
import de.dnpm.dip.coding.{
  Coding,
  CodeSystemProvider
}
import de.dnpm.dip.coding.icd.{
  ICD10GM
}
import de.dnpm.dip.model.{
  Age,
  Diagnosis,
  Site,
  Interval,
  LeftClosedRightOpenInterval
}



trait ReportingOps
{

  import scala.util.chaining._


  type Distribution[+T] =
    Seq[ConceptCount[T]]

  type DistributionsBy[+C,+T] =
    Seq[Entry[C,Distribution[T]]]



  def mean[T: Numeric](ts: Iterable[T]): Double =
    if (ts.nonEmpty)
      Numeric[T].toDouble(ts.sum)/ts.size
    else 
      0.0

/*
  def mean[T: Numeric]: PartialFunction[Iterable[T],Double] = {
    case ts if ts.nonEmpty => Numeric[T].toDouble(ts.sum)/ts.size
  }
*/

  def median[T: Numeric]: PartialFunction[Seq[T],Double] = {
    case ts if ts.nonEmpty =>
      ts.sorted
        .pipe {
          case tts if tts.size % 2 == 0 =>
            val idx = tts.size/2 - 1 
            mean(tts.slice(idx,idx+2))

          case tts =>
            val idx = (tts.size+1)/2 - 1 
            Numeric[T].toDouble(tts(idx))
        }
  }


  def binnedDistribution[T](
    values: Seq[T],
    binSize: T,
  )(
    implicit num: Numeric[T]
  ): Distribution[Interval[T]] = {

    values match {

      case ts if ts.nonEmpty =>

        val min = ts.min
      
        val max = ts.max

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
                ts count (range.contains)
              )
          )

      case _ => Seq.empty

    }

  }


  def ageDistribution(
    values: Seq[Age],
    step: Int = 5
  ): Distribution[Interval[Int]] = {

    import Interval._  // for syntax "x isIn interval"
  
    values match {
      case ages if ages.nonEmpty =>

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

        LazyList
          .unfold(min)(
            l => (l + step) match {
              case r if r <= max =>
                Some(LeftClosedRightOpenInterval(l,r),r)
              case _ => None
            }
          )
          .map(
            range =>
              ConceptCount(
                range,
                ages count (_.value.toInt isIn range)
              )
          )

      case _ => Seq.empty
    }

  }

/*
  def ageDistribution(
    ages: Seq[Age],
    step: Int = 5
  ): Distribution[Interval[Int]] = {

    import Interval._  // for syntax "x isIn interval"
  
    (
      for {

        // Get minimum age, rounded down to multiple of step size
        min <-
          ages.minOption
            .map(_.value.toInt)
            .map(n => n - (n % step))
      
        // Get maximum age, rounded up to multiple of step size
        max <-
          ages.maxOption
            .map(_.value.toInt)
            .map(n => n + (n % step))

        distribution = 
          LazyList
            .from(min,step)
            .takeWhile(_ + step <= max)
            .map {
              left =>
          
                val range =
                  LeftClosedRightOpenInterval(left, left + step)
          
                ConceptCount(
                  range,
                  ages count (_.value.toInt isIn range)
                )
            }

      } yield distribution
    )
    .getOrElse(Seq.empty)

  }
*/

  def distributionBy[T,U](
    ts: Seq[T]
  )(
    grp: T => U
  ): Distribution[U] =
    ts.groupBy(grp)
      .map {
        case (u,seq) =>
          ConceptCount(
            u,
            seq.size,
            None
          )
      }
      .toSeq
      .sorted


  def distribution[T](
    ts: Seq[T]
  ): Distribution[T] =
    distributionBy(ts)(identity)


  def distributionsOn[A,C,T](
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
          distribution(ts)
        )
    }
    .toSeq


  def distributionByParent[T](
    ts: Seq[T]
  )(
    parent: T => T
  ): Distribution[T] =
    ts.groupBy(parent)
      .map {
        case (p,children) =>
          ConceptCount(
            p,
            children.size,
            Some(distribution(children))
          )
      }
      .toSeq
      .sorted

}

object ReportingOps extends ReportingOps
