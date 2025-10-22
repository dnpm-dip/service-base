package de.dnpm.dip.service.mvh


import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import play.api.libs.json.Json


class ConsentTests extends AnyFlatSpec with Matchers 
{

  private def readConsentFile(f: String): ResearchConsent =
    Json.fromJson[ResearchConsent](
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

    assert(ResearchConsent.isGiven(consent))

  }


  it must "have correctly worked on provisions spread across multiple Consent resources" in {

    assert(ResearchConsent.isGiven(partialConsents))

  }

}
