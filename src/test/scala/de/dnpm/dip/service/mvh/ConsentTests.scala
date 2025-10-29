package de.dnpm.dip.service.mvh


import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import play.api.libs.json.Json


class ConsentTests extends AnyFlatSpec with Matchers 
{

  private def readConsentFile(f: String): BroadConsent =
    Json.fromJson[BroadConsent](
      Json.parse(
        this.getClass.getClassLoader.getResourceAsStream(f)
      )
    )
    .get


  val consent = readConsentFile("consent.json")

  val partialConsents =
    List(
      readConsentFile("partial_consent1.json"),
      readConsentFile("partial_consent2.json"),
      readConsentFile("partial_consent3.json")
    )



  "Consent check" must "have correctly worked on bundled provisions within one Consent resource" in {

    assert(BroadConsent.permitsResearchUse(consent))

  }


  it must "have correctly worked on provisions spread across multiple Consent resources" in {

    assert(BroadConsent.permitsResearchUse(partialConsents))

  }

}
