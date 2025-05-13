package de.dnpm.dip.service.query


import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers._
import org.scalatest.Inspectors._
import de.dnpm.dip.model.Age
import de.dnpm.dip.service.Distribution


class DistributionTests extends AnyFlatSpec
{


  "Age Distribution" must "be correctly computed" in {

    val dists =
      List(
        List(40d),
        List(42d),
        List(34d,42d,56d,63d),
        List(30d,42d,33d,56d,65d)
      )
      .map(_.map(Age(_)))
      .map(ages => ages -> Distribution.of(ages))

    forAll (dists){
      case ages -> dist =>
        dist.elements.map(_.value.count).sum mustBe ages.size
    }
  }

}
