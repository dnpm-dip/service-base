package de.dnpm.dip.service.query


import java.time.LocalDateTime
import cats.data.NonEmptyList
import cats.Semigroup
import play.api.libs.json.{
  Json,
  JsObject,
  JsValue,
  Format,
  OFormat,
  Writes,
  OWrites,
  Reads
}
import de.dnpm.dip.coding.Coding
import de.dnpm.dip.model.Site



sealed trait Report[T]
{
  val compiledOn: LocalDateTime
  val scope: Report.Scope.Value
  val data: T 
}

object Report
{

  object Scope extends Enumeration
  {
    val Local  = Value("local")
    val Global = Value("global")

    implicit val format: Format[Scope.Value] =
      Json.formatEnum(this)
  }

}


final case class LocalReport[T]
(
  compiledOn: LocalDateTime,
  site: Coding[Site],
  data: T
)
extends Report[T]
{
  val scope = Report.Scope.Local
}

final case class GlobalReport[T]
(
  compiledOn: LocalDateTime,
  sites: List[Coding[Site]],
  data: T,
  components: Option[Seq[LocalReport[T]]]
)
extends Report[T]
{
  val scope = Report.Scope.Global
}

object LocalReport
{
  implicit def writes[T: Writes]: OWrites[LocalReport[T]] =
    Json.writes[LocalReport[T]]
      .transform(
        _ + ("scope" -> Json.toJson(Report.Scope.Local))
      )

  implicit def reads[T: Reads]: Reads[LocalReport[T]] =
    Json.reads[LocalReport[T]]
}

object GlobalReport
{

  def from[T](
    parts: NonEmptyList[LocalReport[T]]
  )(
    implicit sg: Semigroup[T]
  ): GlobalReport[T] =
    GlobalReport(
      LocalDateTime.now,
      parts.map(_.site).toList,
      parts.map(_.data).reduce,
      Some(parts.toList)
    )


  implicit def writes[T: Writes]: OWrites[GlobalReport[T]] =
    Json.writes[GlobalReport[T]]
      .transform(
        _ + ("scope" -> Json.toJson(Report.Scope.Global))
      )

  implicit def reads[T: Reads]: Reads[GlobalReport[T]] =
    Json.reads[GlobalReport[T]]
}
