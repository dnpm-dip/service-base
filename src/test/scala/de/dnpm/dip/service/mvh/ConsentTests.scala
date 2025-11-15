package de.dnpm.dip.service.mvh


import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.Inspectors._
import org.scalatest.OptionValues._
import de.dnpm.dip.model.{
  Id,
  Patient
}
import play.api.libs.json.Json


class ConsentTests extends AnyFlatSpec with Matchers 
{

  private def readConsent(f: String): BroadConsent =
    Json.fromJson[BroadConsent](
      Json.parse(
        this.getClass.getClassLoader.getResourceAsStream(f)
      )
    )
    .get


  val consent = readConsent("consent.json")

  val partialConsents =
    List(
      readConsent("partial_consent1.json"),
      readConsent("partial_consent2.json"),
      readConsent("partial_consent3.json")
    )



  "Consent check" must "have correctly worked on bundled provisions within one Consent resource" in {

    BroadConsent.permitsResearchUse(consent) mustBe true

  }


  it must "have correctly worked on provisions spread across multiple Consent resources" in {

    BroadConsent.permitsResearchUse(partialConsents) mustBe true

  }


  "Deidentification" must "have worked as expected" in {

    implicit val id = Id[Patient]("DummyPatientId")

    forAll(consent :: partialConsents){ bc => 

      val deidentifiedConsent @ OriginalBroadConsent(json) = MVHService.deidentify(bc)
      
      // Consent.id must have been removed
      json.value.get("id") must not be defined
      
      // Id[Patient] on the Consent.patient reference must have been replaced
      deidentifiedConsent.patient.value.id mustBe id

    }
  }

}
