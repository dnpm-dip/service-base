package de.dnpm.dip.service.validation


import scala.util.Random
import scala.concurrent.Future
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.must.Matchers
import de.dnpm.dip.model.NGSReport.Type.{ 
  Exome,
  GenomeShortRead,
  GenomeLongRead,
  Panel
}
import de.ekut.tbi.generators.Gen
import de.dnpm.dip.service.{
  DataUpload,
  DummyPatientRecord,
  Gens
}
import de.dnpm.dip.service.mvh.Submission
import ValidationService.{ 
  DataValid,
  Validate,
  UnacceptableIssuesDetected
}

class ValidationServiceTests extends AsyncFlatSpec with Matchers
{

  implicit val rnd: Random = new Random(42)


  val admissibleSequencingTypes = Set(GenomeShortRead,GenomeLongRead)

  val service =
    new FakeValidationService[Future,DummyPatientRecord](
      admissibleSequencingTypes
    )


  val admissibleUploads: Gen[DataUpload[DummyPatientRecord]] =
    for {
      record <- Gens.genDummyPatientRecord(admissibleSequencingTypes)
      metadata <- Gens.genMetadata(record,Submission.Type.Initial,true)
    } yield DataUpload(record,Some(metadata))


  val nonAdmissibleUploads: Gen[DataUpload[DummyPatientRecord]] =
    for {
      record <- Gens.genDummyPatientRecord(Set(Exome,Panel))
      metadata <- Gens.genMetadata(record,Submission.Type.Initial,true)
    } yield DataUpload(record,Some(metadata))



  "Validation" must "have failed for PatientRecord with inadmissible sequencing types" in { 

    for { 
      outcome <- service ! Validate(nonAdmissibleUploads.next)
    } yield outcome must matchPattern { case Left(_: UnacceptableIssuesDetected) => }

  }


  it must "have succeeded for PatientRecord with admissible sequencing types" in { 

    for { 
      outcome <- service ! Validate(admissibleUploads.next)
    } yield outcome must matchPattern { case Right(_: DataValid[_]) => }

  }

}
