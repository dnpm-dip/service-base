package de.dnpm.dip.service.query


import java.time.LocalDateTime
import de.dnpm.dip.coding.{
  Coding,
  CodeSystemProvider
}
import de.dnpm.dip.model.{
  Age,
  Interval,
  LeftClosedRightOpenInterval
}



trait ReportingOps
{

  import scala.util.chaining._


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

  private def even(n: Int) = n % 2 == 0

  def median[T: Numeric]: PartialFunction[Seq[T],Double] = {

    case ts if ts.nonEmpty =>
      ts.sorted
        .pipe {
          case tts if even(tts.size) =>
            val idx = tts.size/2 - 1 
            mean(tts.slice(idx,idx+2))

          case tts =>
            val idx = (tts.size+1)/2 - 1 
            Numeric[T].toDouble(tts(idx))
        }
  }


}
object ReportingOps extends ReportingOps
