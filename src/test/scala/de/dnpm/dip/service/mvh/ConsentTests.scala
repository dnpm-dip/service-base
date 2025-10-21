package de.dnpm.dip.service.mvh


import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import play.api.libs.json.Json



class ConsentTests extends AnyFlatSpec with Matchers 
{

  lazy val consent =
    Json.fromJson[ResearchConsent](
      Json.parse(
        this.getClass.getClassLoader.getResourceAsStream("consent.json")
      )
    )
    .get



  "Consent permission check" must "have correctly worked" in {

    assert(ResearchConsent.isGiven(consent))
  }

}
