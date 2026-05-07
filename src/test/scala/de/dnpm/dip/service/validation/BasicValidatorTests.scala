package de.dnpm.dip.service.validation


import scala.util.chaining._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers._
import org.scalatest.Inspectors._
import Issue.Path
import de.ekut.tbi.validation.dsl.validate
import de.dnpm.dip.coding.Code
import de.dnpm.dip.coding.hgvs.HGVS


class BasicValidatorTests extends AnyFlatSpec with Validators
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



/*
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
*/

  "HGVS protein change validation" must "have succeeded on correct codes" in { 

    val proteinChanges =
      Seq(
        "p.Gly12Cys",
        "p.Gly12Ter",
        "p.(Arg727Ser)",
        "p.Trp24Ter",
        "p.Trp24*",
        "p.Gly12*",
        "p.Cys188",
        "p.(Met1?)",
        "p.Trp24=/Cys",
        "p.Arg97Profs*23",
        "p.Ile327Argfs*?",
        "p.(His321Leufs*3)",
        "p.(Asn47delinsSerSerTer)",
        "p.[Ser68Arg;Asn594del]",
        "p.[(Asn158Asp)(;)(Asn158Ile)]^[(Asn158Val)]",
        "p.Ala2[10];[11]",
        "p.Tyr4*",
        "p.Tyr4TerfsTer1",
        "p.?",
        "p.0?",
        "p.(=)"
      )
      .map(Code[HGVS.Protein](_))

    forAll(proteinChanges)(code => validate(code).isValid mustBe true)

  }

  it must "have failed on incorrect codes" in { 

    val proteinChanges =
      Seq(
        "p.G12C",
        "p.*",
        "p.whatever"
      )
      .map(Code[HGVS.Protein](_))

    forAll(proteinChanges)(code => validate(code).isValid mustBe false)

  }

}
