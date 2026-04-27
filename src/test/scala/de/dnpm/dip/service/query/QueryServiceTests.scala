package de.dnpm.dip.service.query


import scala.concurrent.Future
import org.scalatest.flatspec.AsyncFlatSpec
import de.dnpm.dip.model.{
  Id,
  Site
}
import de.dnpm.dip.service.DummyPatientRecord


class QueryServiceTests extends AsyncFlatSpec
{ 

  val service =
    new FakeQueryService[Future,DummyPatientRecord](
      federatedQueriesActive = false
    )

  "QueryService with federatedQueriesActive = false" must "have rejected FederatedQuery" in { 

    for {
      result <- service ! FederatedQuery[DummyQueryCriteria,DummyPatientRecord](
        Site.local,
        Querier("Dummy"),
        None
      )

    } yield assert(result.isLeft)
  }


  it must "have rejected PatientRecordRequest" in { 

    for {
      result <- service ! PatientRecordRequest[DummyPatientRecord](
        Site.local,
        Querier("Dummy"),
        Id("Dummy"),
        None
      )

    } yield assert(result.isLeft)
  }

}
