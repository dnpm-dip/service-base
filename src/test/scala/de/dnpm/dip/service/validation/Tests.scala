package de.dnpm.dip.service.validation


import scala.util.chaining._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers._
import org.scalatest.Inspectors._
import Issue.Path
import play.api.libs.json.Json
import de.dnpm.dip.service.mvh.BroadConsent
import de.ekut.tbi.validation.dsl.validate
import de.dnpm.dip.model.{
  Id,
  Patient
}
import de.dnpm.dip.coding.Code
import de.dnpm.dip.coding.hgvs.HGVS


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


  "HGVS protein change validation" must "have succeeded on correct codes" in { 

    val proteinChanges =
      Seq(
        "p.Gly12Cys",
        "p.(=)"
      )
      .map(Code[HGVS.Protein](_))

    forAll(proteinChanges)(code => validate(code).isValid mustBe true)

  }

  it must "have failed on incorrect codes" in { 

    val proteinChanges =
      Seq(
        "p.G12C",
        "p.whatever"
      )
      .map(Code[HGVS.Protein](_))

    forAll(proteinChanges)(code => validate(code).isValid mustBe false)

  }

}
