package de.dnpm.dip.service.mvh


import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.Inspectors
import org.scalatest.matchers.must.Matchers._


class TanExtractorTests
extends AnyFlatSpec
with Inspectors
{

  "TAN extraction" must "have worked" in {

    val tan = "3F36FDB56A87D982A0DBCF357E919233A49D4B9855AA483F4F339D4606F3500"

    val TAN(extractedTan) = s"SomeFilePrefix_TAN_$tan.json"

    assert(extractedTan.value == tan)
  }

}
