package de.dnpm.dip.service.query


import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers._
import org.scalatest.Inspectors._
import de.dnpm.dip.model.Age


class DistributionTests extends AnyFlatSpec
{


  "Age Distribution" must "be correctly computed" in {

    val dists =
      List(
        List(40),
        List(42),
        List(34,42,56,63),
        List(30,42,33,56,65)
      )
      .map(_.map(Age(_)))
      .map(ages => ages -> Distribution.of(ages))

    forAll (dists){
      case ages -> dist =>
        dist.elements.map(_.value.count).sum mustBe ages.size
    }
  }

}
