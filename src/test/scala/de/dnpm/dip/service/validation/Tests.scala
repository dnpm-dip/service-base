package de.dnpm.dip.service.validation


import scala.util.chaining._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers._
import Issue.Path
import play.api.libs.json.Json
import de.dnpm.dip.service.mvh.BroadConsent
import de.ekut.tbi.validation.dsl.validate
import de.dnpm.dip.model.{
  Id,
  Patient
}


class Tests extends AnyFlatSpec with Validators
{

  "Path" must "not contain double slashes '//'" in {

     val path =
       (Path.root / "path" / "to" / "node")
         .pipe(_.toString)
         .pipe(Path.from)
         .pipe(_.toString)

     path must not be (empty)
     path must not include ("//")
  }



  private def readConsent(f: String): BroadConsent =
    Json.fromJson[BroadConsent](
      Json.parse(
        this.getClass.getClassLoader.getResourceAsStream(f)
      )
    )
    .get


  "BroadConsent validation" must "have succeeded" in { 

    val consent = readConsent("consent.json")

    implicit val patId = consent.patient.get.id

    val validation = validate(consent)

    validation.isValid mustBe true
  }


  it must "have failed" in { 

    val consent = readConsent("invalid_consent.json")

    implicit val patId = Id[Patient]("1234")

    val validation = validate(consent)

    validation.isValid mustBe false
  }

}
