package de.dnpm.dip.service.mvh


import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.OptionValues._
import de.dnpm.dip.model.{
  Id,
  Patient
}
import de.dnpm.dip.service.Deidentifier
import play.api.libs.json.{
  Json,
  JsResult
}


class ConsentTests extends AnyFlatSpec with Matchers 
{

  private def readConsent(f: String): JsResult[BroadConsent] =
    Json.fromJson[BroadConsent](
      Json.parse(
        this.getClass.getClassLoader.getResourceAsStream(f)
      )
    )


  val consent = readConsent("consent.json").get


  "Consent check" must "have correctly worked on bundled provisions within one Consent resource" in {

    BroadConsent.permitsResearchUse(consent) mustBe true

  }


  "Deidentifier[BroadConsent]" must "have worked as expected" in {

    implicit val id = Id[Patient]("DummyPatientId")

    val deidentifiedConsent @ WrappedBroadConsent(json) = Deidentifier[BroadConsent].apply(consent)
    
    // Consent.id must have been removed
    json.value.get("id") must not be defined
    
    // Id[Patient] on the Consent.patient reference must have been replaced
    deidentifiedConsent.patient.value.id mustBe id

  }


  "Deserialization of syntactically wrong consent" must "have failed" in { 

    readConsent("syntactically_wrong_consent.json").isError mustBe true

  }

}
