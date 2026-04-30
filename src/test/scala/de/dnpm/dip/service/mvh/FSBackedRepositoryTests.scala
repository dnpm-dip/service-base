package de.dnpm.dip.service.mvh


import java.nio.file.Files.createTempDirectory
import scala.concurrent.Future
import scala.util.Random
import cats.syntax.traverse._
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.must.Matchers._
import de.dnpm.dip.service.DummyPatientRecord
import de.dnpm.dip.service.Gens._
import de.ekut.tbi.generators.Gen
import Submission.Type.Test
import MVHService.{
  Process,
  Saved
}


class FSBackedRepositoryTests extends AsyncFlatSpec 
{

  behavior of "FSBackedRepository"


  implicit val rnd: Random = new Random(42)

  System.setProperty("dnpm.dip.site","UKX:Musterlingen")


  // The test suite is for FSBackedRepository, but it is wrapped in a FakeMVHService for convenience,
  // given that Submissions can only be stored paired with a Submission.Report,
  // the creation logic of which is encapsulated in MVHService
  val service =
    new FakeMVHService[Future,DummyPatientRecord](
      UseCase.MTB,
      new FSBackedRepository[Future,DummyPatientRecord](
        createTempDirectory("fs_backed_repo_test_dir").toFile
      )
    )


  val metadata: Gen[Submission.Metadata] = genMetadata(Test)


  it must "have correctly stored Submissions and allowed their retrieval" in {

    val n = 42

    val submissions =
      List.fill(n)(metadata.next).map(Gen.of[DummyPatientRecord].next -> _)

    for { 
      outcomes <- submissions.traverse { case (record,metadata) => service ! Process(record,metadata) }

      _ = all (outcomes) must matchPattern { case Right(Saved) => }

      loadedSubmissions <- service ? Submission.Filter()

      _ = loadedSubmissions.size mustBe n


      submissionsByTAN <- submissions.map(_._2.transferTAN).traverse(service.submission(_))

      _ = all (submissionsByTAN) must be (defined)

    } yield succeed // If this point is reached, test succeeded

  }

}
