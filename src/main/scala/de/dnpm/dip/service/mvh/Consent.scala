package de.dnpm.dip.service.mvh 

import play.api.libs.json.{
  Json,
  JsObject,
  Format,
}


// leave Consent unstructured (for now)
final case class Consent(value: JsObject) extends AnyVal
object Consent
{
  implicit val format: Format[Consent] =
    Json.valueFormat[Consent]
}

