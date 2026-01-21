package de.dnpm.dip.service


import scala.concurrent.Future
import scala.util.Random
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.must.Matchers._
import org.scalatest.EitherValues._
import org.scalatest.OptionValues._
import de.dnpm.dip.model.{
  Patient,
  Site
}
import de.dnpm.dip.service.validation.FakeValidationService
import de.dnpm.dip.service.mvh.FakeMVHService
import de.dnpm.dip.service.query.{
  FakeQueryService,
  PatientRecordRequest,
  Querier
}
import de.ekut.tbi.generators.Gen
import Gens._
import Orchestrator._


class OrchestratorTests extends AsyncFlatSpec
{

  System.setProperty("dnpm.dip.site","UKX:Musterlingen")


  implicit val rnd: Random =
    new Random(42)


  implicit val patientSetter: (DummyPatientRecord,Patient) => DummyPatientRecord =
    (record,patient) => record.copy(patient = patient)

   
  val validationService =
    new FakeValidationService[Future,DummyPatientRecord]

  val mvhService =
    new FakeMVHService[Future,DummyPatientRecord]

  val queryService =
    new FakeQueryService[Future,DummyPatientRecord]()


  val orchestrator =
    new Orchestrator(
      validationService,
      mvhService,
      queryService
    )

  val querier = Querier("Dummy")

  val record = Gen.of[DummyPatientRecord].next

  lazy val retrievalRequest =
    PatientRecordRequest[DummyPatientRecord](
      Site.local,
      querier,
      record.id,
      None
    )


  "DataUpload processing" must "have succeeded" in { 

    for {
      outcome <- orchestrator ! Process(DataUpload(record,None))

      _ = outcome.value mustBe Saved

      snapshot <- queryService ! retrievalRequest

    } yield snapshot.value.data.id mustBe record.id

  }


  "Data deletion" must "have succeeded" in { 

    for {
      outcome <- orchestrator ! Delete(record.id)
    
      _ = outcome.value mustBe a [Deleted]

      snapshot <- queryService ! retrievalRequest

    } yield snapshot must not be defined

  }

}
