package de.dnpm.dip.service.validation


import scala.util.chaining._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers._
import org.scalatest.EitherValues._
import org.scalatest.Inspectors._
import Issue.Path
import de.ekut.tbi.validation.dsl.validate
import de.dnpm.dip.coding.Code
import de.dnpm.dip.coding.hgvs.HGVS
import de.dnpm.dip.service.mvh.BroadConsent
import play.api.libs.json.Json


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


  private def readConsent(f: String): BroadConsent =
    Json.fromJson[BroadConsent](
      Json.parse(
        this.getClass.getClassLoader.getResourceAsStream(f)
      )
    )
    .get


  "BroadConsent validation" must "have succeeded" in { 

    val consent = readConsent("consent.json")

    val validation = validate(consent)

    validation.isValid mustBe true
  }


  it must "have failed" in { 

    // Wrap in List to test index-based Issue.path info at the same time 
    val consents = List(readConsent("invalid_consent.json"))

    val validation = validateEachWithIndex(consents)

    validation.isValid mustBe false

    forAll(validation.swap.toEither.value.toList)(
      issue => issue.path.toString must include regex "\\[\\d\\]"
    )

  }


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
