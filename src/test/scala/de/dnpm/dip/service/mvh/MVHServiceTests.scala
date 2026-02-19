package de.dnpm.dip.service.mvh


import scala.concurrent.Future
import scala.util.Random
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.EitherValues._
import org.scalatest.matchers.must.Matchers._
import de.dnpm.dip.service.DummyPatientRecord
import de.dnpm.dip.service.Gens._
import de.ekut.tbi.generators.Gen
import MVHService.{
  InvalidSubmissionType,
  Process,
  Saved
}


class MVHServiceTests extends AsyncFlatSpec 
{

  implicit val rnd: Random = new Random(42)

  val service = new FakeMVHService[Future,DummyPatientRecord]

  val initialMetadata = genMetadata(Submission.Type.Initial)
 
  val additionMetadata = genMetadata(Submission.Type.Addition)


  "Submission" must "have been refused for type 'addition' as first upload" in {

    val record = Gen.of[DummyPatientRecord].next

    for { 
      outcome <- service ! Process(record,additionMetadata.next)

    } yield outcome match {
      case Left(_: InvalidSubmissionType) => succeed
      case _ => fail("'addition' submission should have been refused")
    }

  }


  it must "have been refused for two consecutive 'initial' submissions" in {

    val record = Gen.of[DummyPatientRecord].next

    for { 
      outcome1 <- service ! Process(record,initialMetadata.next)
    
      _ = outcome1.value mustBe Saved

      outcome2 <- service ! Process(record,initialMetadata.next)

    } yield outcome2 match {
      case Left(_: InvalidSubmissionType) => succeed
      case _ => fail("2nd 'initial' submission should have been refused")
    }

  }


  it must "have worked for type 'initial' followed by 'addition'" in {

    val record = Gen.of[DummyPatientRecord].next

    for { 
      outcome1 <- service ! Process(record,initialMetadata.next)
    
      _ = outcome1.value mustBe Saved

      outcome2 <- service ! Process(record,additionMetadata.next)

    } yield outcome2.value mustBe Saved

  }

}
