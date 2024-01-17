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
//      .sorted(Ordering[Int].on[ConceptCount[U]](_.value).reverse)


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
//      .sorted(Ordering[Int].on[ConceptCount[T]](_.value).reverse)

}

object ReportingOps extends ReportingOps
