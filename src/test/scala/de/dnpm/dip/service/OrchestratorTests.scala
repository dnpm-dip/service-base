package de.dnpm.dip.service


import scala.concurrent.Future
import scala.util.Random
import cats.syntax.traverse._
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.must.Matchers._
import org.scalatest.EitherValues._
import org.scalatest.OptionValues._
import de.dnpm.dip.model.{
  Patient,
  Site
}
import de.dnpm.dip.service.validation.FakeValidationService
import de.dnpm.dip.service.mvh.{
  FakeMVHService,
  Submission
}
import de.dnpm.dip.service.query.{
  FakeQueryService,
  PatientRecordRequest,
  Querier
}
import de.dnpm.dip.service.controlling.FederatedControllingInfo
import de.ekut.tbi.generators.Gen
import Gens._
import Orchestrator._


class OrchestratorTests extends AsyncFlatSpec
{

  System.setProperty("dnpm.dip.site","UKX:Musterlingen")


  implicit val rnd: Random = new Random(42)


  implicit val patientSetter: (DummyPatientRecord,Patient) => DummyPatientRecord =
    (record,patient) => record.copy(patient = patient)

   
  val validationService = new FakeValidationService[Future,DummyPatientRecord]

  val mvhService = new FakeMVHService[Future,DummyPatientRecord]()

  val queryService = new FakeQueryService[Future,DummyPatientRecord](true)


  val orchestrator =
    new Orchestrator(
      validationService,
      mvhService,
      queryService
    )(
      FakeConnector[Future]
    )


  val dataUpload: Gen[DataUpload[DummyPatientRecord]] =
    for {
      record <- Gen.of[DummyPatientRecord]
      metadata <- genMetadata(record,Submission.Type.Initial,true)
    } yield DataUpload(
      record,
      Some(metadata)
    )

  val initialUpload = dataUpload.next

  val record = initialUpload.record

  val subsequentUpload =
    initialUpload.copy(
      metadata = Some(
        genMetadata(record, Submission.Type.Addition,false).next
      )
    )

  lazy val retrievalRequest =
    PatientRecordRequest[DummyPatientRecord](
      Site.local,
      Querier("Dummy"),
      record.id,
      None
    )


  "Data Upload" must "have worked as expected on initial submission with Research Consent" in { 

    for {

      outcome <- orchestrator ! Process(initialUpload)

      _ = outcome.value mustBe Saved

      submissionReport <- mvhService submissionReport initialUpload.metadata.get.transferTAN

      _ = submissionReport.value.patient mustBe record.id

      snapshot <- queryService ! retrievalRequest

      // The data must have been saved in the queryService
      _ = snapshot.value.value.data.id mustBe record.id

    } yield succeed // If not failed before, test passed

  }


  it must "have worked as expected on subsequent submission without Research Consent" in { 

    for {

      outcome <- orchestrator ! Process(subsequentUpload)

      _ = outcome.value mustBe Saved

      submissionReport <- mvhService submissionReport subsequentUpload.metadata.get.transferTAN

      _ = submissionReport.value.patient mustBe record.id

      snapshot <- queryService ! retrievalRequest

      // Now the data must have been deleted from the queryService
      _ = snapshot.value must not be defined

    } yield succeed // If not failed before, test passed

  }


  "Data deletion" must "have succeeded" in { 

    for {
      outcome <- orchestrator ! Delete(record.id)
    
      _ = outcome.value mustBe a [Deleted]

      submissionReport <- mvhService submissionReport initialUpload.metadata.get.transferTAN

      _ = submissionReport must not be defined
    
      snapshot <- queryService ! retrievalRequest

     _ = snapshot.value must not be defined

    } yield succeed // If not failed before, test passed

  }




  "LocalControllingInfo" must "have been correctly compiled" in { 

    val n = 42

    for {

      initialInfo <- orchestrator.localControllingInfo(criteria = None)

      _ = initialInfo.mvGenomSeq.total mustBe 0
      _ = initialInfo.query.total mustBe 0

      dataUploads = List.fill(n)(dataUpload.next)

      uploadOutcomes <- dataUploads.traverse(orchestrator ! Process(_))

      _ = all (uploadOutcomes) must matchPattern { case Right(Saved) => }

      postUploadInfo <- orchestrator.localControllingInfo(criteria = None)

      _ = postUploadInfo.mvGenomSeq.total mustBe n
      _ = postUploadInfo.query.total mustBe n

    } yield succeed

  }


  "FederatedDataCounts" must "have been correctly compiled" in { 

    for {

      result <- orchestrator.federatedControllingInfo(
        None,
        Some(Set(Site.local))
      )

      FederatedControllingInfo(_,sites,criteria,_,_,components,errors) = result.value

      _ = sites must have size 1 

      _ = errors must not be (defined)

    } yield succeed

  }

}
