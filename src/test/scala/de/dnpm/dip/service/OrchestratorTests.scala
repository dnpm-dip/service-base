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

  val dataUpload = Gen.of[DataUpload[DummyPatientRecord]].next
  lazy val record = dataUpload.record


  lazy val retrievalRequest =
    PatientRecordRequest[DummyPatientRecord](
      Site.local,
      querier,
      record.id,
      None
    )


  "DataUpload processing" must "have succeeded" in { 

    for {
      outcome <- orchestrator ! Process(dataUpload)

      _ = outcome.value mustBe Saved

      submissionReport <- mvhService ? dataUpload.metadata.get.transferTAN

      snapshot <- queryService ! retrievalRequest

      _ = submissionReport.value.patient mustBe record.id

    } yield snapshot.value.data.id mustBe record.id

  }


  "Data deletion" must "have succeeded" in { 

    for {
      outcome <- orchestrator ! Delete(record.id)
    
      _ = outcome.value mustBe a [Deleted]

      submissionReport <- mvhService ? dataUpload.metadata.get.transferTAN

      snapshot <- queryService ! retrievalRequest

      _ <- submissionReport must not be defined

    } yield snapshot must not be defined

  }

}
