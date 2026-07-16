package de.dnpm.dip.service.mvh


import java.time.LocalDateTime
import java.nio.file.Files.createTempDirectory
import scala.concurrent.Future
import scala.util.Random
import cats.syntax.traverse._
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.must.Matchers._
import de.dnpm.dip.model.Period
import de.dnpm.dip.service.DummyPatientRecord
import de.dnpm.dip.service.Gens._
import de.ekut.tbi.generators.Gen
import Submission.Type.Test
import MVHService.{
  Delete,
  Deleted,
  Process,
  Saved
}


class FSBackedRepositoryTests extends AsyncFlatSpec 
{

  behavior of "FSBackedRepository"


  implicit val rnd: Random = new Random(42)

  System.setProperty("dnpm.dip.site","UKX:Musterlingen")


  val dataDir = createTempDirectory("fs_backed_repo_test_dir").toFile
  dataDir.deleteOnExit


  // The test suite is for FSBackedRepository, but it is wrapped in a FakeMVHService for convenience,
  // given that Submissions can only be stored paired with a Submission.Report,
  // the creation logic of which is encapsulated in MVHService
  val service =
    new FakeMVHService[Future,DummyPatientRecord](
      UseCase.MTB,
      new FSBackedRepository[Future,DummyPatientRecord](dataDir)
    )


  val metadata: Gen[Submission.Metadata] = genMetadata(Test)


  it must "have correctly stored Submissions, allowed their retrieval and then deletion" in {

    val n = 42

    val submissions = List.fill(n)(metadata.next).map(Gen.of[DummyPatientRecord].next -> _)

    val start = LocalDateTime.now

    for { 

      saveOutcomes <- submissions.traverse { case (record,metadata) => service ! Process(record,metadata) }

      _ = all (saveOutcomes) must matchPattern { case Right(Saved) => }

      // For each Submission, a Submission and Submission.Report file must have been created, hence the factor of 2
      _ = dataDir.listFiles.size mustBe 2*n

      loadedSubmissions <- service ? Submission.Filter()

      _ = loadedSubmissions.size mustBe n


      submissionsByTAN <- submissions.map(_._2.transferTAN).traverse(service.submission(_))

      _ = all (submissionsByTAN) must be (defined)


      deletionOutcomes <- submissions.map(_._1.patient.id).traverse(service ! Delete(_))

      _ = all (deletionOutcomes) must matchPattern { case Right(Deleted) => }


      submissionsAfterDeletion <- submissions.map(_._2.transferTAN).traverse(service submission _)

      _ = all (submissionsAfterDeletion) must be (empty)

      _ = dataDir.listFiles((_,name) => name contains "MVH_DummyPatientRecord") mustBe empty


      deletionEvents <- service.deletionEvents(Period(start,LocalDateTime.now))

      _ = deletionEvents.size mustBe n

    } yield succeed // If this point is reached, test succeeded

  }

}
