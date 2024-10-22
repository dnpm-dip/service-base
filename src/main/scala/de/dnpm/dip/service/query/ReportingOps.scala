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


  def mean[T: Numeric](ts: Iterable[T]): Option[Double] =
    if (ts.nonEmpty)
      Some(Numeric[T].toDouble(ts.sum)/ts.size)
    else
      None

  def mean[T: Numeric](t: T, ts: T*): Double =
    mean(t +: ts).get


  private def even(n: Int) = n % 2 == 0

  def median[T: Numeric](ts: Seq[T]): Option[Double] =
    if (ts.nonEmpty)
      Some(
        ts.sorted
          .pipe {
            case tts if even(tts.size) =>
              val idx = tts.size/2 - 1 
              mean(tts(idx),tts(idx+1))
        
            case tts =>
              val idx = (tts.size+1)/2 - 1 
              Numeric[T].toDouble(tts(idx))
          }
      )
    else
      None 
  
  def median[T: Numeric](t: T, ts: T*): Double =
    median(t +: ts).get

}

object ReportingOps extends ReportingOps
