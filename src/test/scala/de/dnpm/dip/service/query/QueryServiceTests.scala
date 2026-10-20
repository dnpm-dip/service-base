package de.dnpm.dip.service.query

import java.time.LocalDate
import java.time.Month.JANUARY
import scala.concurrent.Future
import scala.util.Random
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.must.Matchers._
import org.scalatest.EitherValues._
import cats.syntax.traverse._
import de.dnpm.dip.coding.Coding
import de.dnpm.dip.model.{
  Id,
  Period,
  Site
}
import de.dnpm.dip.service.DummyPatientRecord
import QueryService.Save
import BaseQueryService.FEDERATED_QUERIES_INACTIVE
import de.ekut.tbi.generators.Gen
import de.dnpm.dip.service.Gens.genDummyPatientRecordWithoutNGS


class QueryServiceTests extends AsyncFlatSpec
{ 

  implicit val rnd: Random = new Random()

  val service = new FakeQueryService[Future,DummyPatientRecord](federatedQueriesActive = false)

  implicit val querier: Querier = Querier("Dummy")

  List.fill(42)(Gen.of[DummyPatientRecord].next).traverse(service ! Save(_)) 


  "QueryService with federatedQueriesActive = false" must "have rejected FederatedQuery" in { 

    for {
      result <- service ! FederatedQuery[DummyQueryCriteria,DummyPatientRecord](
        Site.local,
        querier,
        None
      )

    } yield result mustBe Left(FEDERATED_QUERIES_INACTIVE)

  }


  it must "have rejected PatientRecordRequest" in { 

    for {
      result <- service ! PatientRecordRequest[DummyPatientRecord](
        Site.local,
        querier,
        Id("Dummy"),
        None
      )

    } yield result mustBe Left(FEDERATED_QUERIES_INACTIVE)

  }

  it must "have correctly performed Query.Update" in { 

    for { 
      submitOutcome <- service ! Query.Submit(
        mode     = Coding(Query.Mode.Local),
        sites    = None,
        criteria = Some(
          DummyQueryCriteria(
            Some(Period(LocalDate.of(1970,JANUARY,1), LocalDate.now.plusYears(1)))
          )
        )
      )

      query = submitOutcome.value

      updateOutcome <- service ! Query.Update(
        id       = query.id,
        mode     = None,
        sites    = None,
        criteria = Some(
          DummyQueryCriteria(
            Some(Period(LocalDate.of(1980,JANUARY,1), LocalDate.now.plusYears(2)))
          )
        )
      )
      
      updatedQuery = updateOutcome.value

    } yield updatedQuery.criteria must not equal (query.criteria)
  }

}
