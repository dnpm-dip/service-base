package de.dnpm.dip.service.query


import org.scalatest.flatspec.AnyFlatSpec


import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers._
import de.dnpm.dip.coding.{
  Coding,
  CodeSystemProvider
}


class CodeSystemTests extends AnyFlatSpec
{

  "Loading CodeSystems dynamically via CodeSystemProvider" must "have worked" in {

    CodeSystemProvider
      .getInstances[cats.Id]
      .map(_.uri)
      .toList must contain (
        Coding.System[Query.Mode.Value].uri,
      )

  }

}

