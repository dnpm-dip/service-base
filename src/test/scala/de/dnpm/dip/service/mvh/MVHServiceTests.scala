package de.dnpm.dip.service.mvh


import scala.concurrent.Future
import scala.util.Random
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.EitherValues._
import org.scalatest.Inspectors._
import org.scalatest.matchers.must.Matchers._
import cats.syntax.traverse._
import de.dnpm.dip.model.{
  EpisodeOfCare,
  Period,
  Reference
}
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

  System.setProperty("dnpm.dip.site","UKX:Musterlingen")

  // Tests below require updating the EpisodeOfCare reference on Submission.Metadata,
  // so provide this as an extension method for better readability
  implicit class MetadataExtensions(val metadata: Submission.Metadata)
  {
    def withEpisodeOfCare(episode: EpisodeOfCare) =
      metadata.copy(
        episodeOfCare = Some(Reference.to(episode))
      )
  }



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
      outcomes <- nonInitialMetadata.next.traverse(service ! Process(record,_))

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

      outcomes <- nonInitialMetadata.next.traverse(service ! Process(record,_))

    } yield forAll(initialOutcome :: outcomes){_.value mustBe Saved }

  }


  it must "have been refused for non-'initial' types given a new EpisodeOfCare without a new 'initial' submission" in {

    val record = Gen.of[DummyPatientRecord].next

    for { 
      outcome1 <- service ! Process(record,initialMetadata.next.withEpisodeOfCare(record.episodesOfCare.head))
    
      _ = outcome1.value mustBe Saved

      newEpisode = genEpisodeOfCare(record.patient).next.copy(
        period = Period(record.episodesOfCare.head.period.start plusMonths 3)
      )

      recordWithNewEpisode = record.copy(
        episodesOfCare = newEpisode :: record.episodesOfCare
      )

      outcomes2 <-
        nonInitialMetadata.next
          .map(_.withEpisodeOfCare(newEpisode))
          .traverse(service ! Process(recordWithNewEpisode,_))

    } yield all (outcomes2) must matchPattern { case Left(_: InvalidSubmissionType) => }

  }


  it must "have worked for a second 'initial' submission given a new EpisodeOfCare" in {

    val record = Gen.of[DummyPatientRecord].next

    for { 
      outcome1 <- service ! Process(record,initialMetadata.next.withEpisodeOfCare(record.episodesOfCare.head))
    
      _ = outcome1.value mustBe Saved

      newEpisode = genEpisodeOfCare(record.patient).next.copy(
        period = Period(record.episodesOfCare.head.period.start plusMonths 3)
      )

      recordWithNewEpisode = record.copy(
        episodesOfCare = newEpisode :: record.episodesOfCare
      )

      outcome2 <- service ! Process(recordWithNewEpisode,initialMetadata.next.withEpisodeOfCare(newEpisode))

    } yield outcome2.value mustBe Saved

  }



}
