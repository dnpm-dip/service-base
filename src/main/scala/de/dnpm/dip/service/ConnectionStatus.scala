package de.dnpm.dip.service


import scala.util.{
  Either,
  Left,
  Right
}
import de.dnpm.dip.coding.Coding
import de.dnpm.dip.model.Site
import play.api.libs.json.{
  Json,
  Format,
  OFormat
}


final case class ConnectionStatus
(
  site: Coding[Site],
  status: ConnectionStatus.Value,
  details: String
)

object ConnectionStatus extends Enumeration
{

  val Online = Value("online")
  val Offline = Value("offline")


  def from[T](
    resultsBySite: Map[Coding[Site],Either[String,T]]
  ): Seq[ConnectionStatus] =
    resultsBySite.map {
      case (site,result) =>
        result match {
          case Right(_) =>
            ConnectionStatus(
              site,
              Online,
              "-"
            )

          case Left(msg) =>
            ConnectionStatus(
              site,
              Offline,
              msg
            )
        }
    }
    .toSeq


  implicit val formatValue: Format[Value] =
    Json.formatEnum(this)

  implicit val format: OFormat[ConnectionStatus] =
    Json.format[ConnectionStatus]

}
