package de.dnpm.dip.service.mvh


import scala.concurrent.Future
import scala.util.Random
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.EitherValues._
import org.scalatest.Inspectors._
import org.scalatest.matchers.must.Matchers._
import de.dnpm.dip.service.DummyPatientRecord
import de.dnpm.dip.service.Gens._
import de.ekut.tbi.generators.Gen
import MVHService.{
  InvalidSubmissionType,
  Process,
  Saved
}
import Submission.Type.{
  Initial,
  Test
}


class MVHServiceTests extends AsyncFlatSpec 
{

  implicit val rnd: Random = new Random(42)

  val service = new FakeMVHService[Future,DummyPatientRecord]


  // Generator for Metadata with submission type 'initial'
  val initialMetadata: Gen[Submission.Metadata] =
    genMetadata(Initial)

  // Generator for Metadata with all submission types except 'initial'/'test'
  val nonInitialMetadata: Gen[List[Submission.Metadata]] =
    Gen.oneOfEach(
      (Submission.Type.values - Initial - Test).toList.map(genMetadata(_))
    )

  "Submission" must "have been refused for all non-'initial' types as first upload" in {

    val record = Gen.of[DummyPatientRecord].next

    for { 
      outcomes <- Future.traverse(nonInitialMetadata.next)(service ! Process(record,_))

    } yield all (outcomes) must matchPattern { case Left(_: InvalidSubmissionType) => }

  }


  it must "have been refused for two consecutive 'initial' submissions" in {

    val record = Gen.of[DummyPatientRecord].next

    for { 
      outcome1 <- service ! Process(record,initialMetadata.next)
    
      _ = outcome1.value mustBe Saved

      outcome2 <- service ! Process(record,initialMetadata.next)

    } yield outcome2 must matchPattern { case Left(_: InvalidSubmissionType) => }

  }


  it must "have worked for type 'initial' followed by all non-'initial' types'" in {

    val record = Gen.of[DummyPatientRecord].next

    for { 
      initialOutcome <- service ! Process(record,initialMetadata.next)

      outcomes <- Future.traverse(nonInitialMetadata.next)(service ! Process(record,_))

    } yield forAll(initialOutcome :: outcomes){_.value mustBe Saved }

  }

}
