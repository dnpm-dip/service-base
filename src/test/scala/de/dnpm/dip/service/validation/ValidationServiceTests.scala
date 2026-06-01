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
import de.dnpm.dip.service.{
  DummyPatientRecord,
  Gens
}
import de.dnpm.dip.service.mvh.Submission
import ValidationService.{ 
  DataValid,
  Validate,
  UnacceptableIssuesDetected
}
import Submission.Type.FollowUp


class ValidationServiceTests extends AsyncFlatSpec with Matchers// with Validators
{

  implicit val rnd: Random = new Random(42)


  val admissibleSequencingTypes = Set(GenomeShortRead,GenomeLongRead)

  val service =
    new FakeValidationService[Future,DummyPatientRecord](
      admissibleSequencingTypes
    )


  val admissibleUploads =
    Gens.genDataUpload(sequencingTypes = admissibleSequencingTypes)

  val nonAdmissibleUploads =
    Gens.genDataUpload(sequencingTypes = Set(Exome,Panel))

  val followUpUploads =
    Gens.genDataUpload(
      submissionType = FollowUp,
      sequencingTypes = admissibleSequencingTypes
    )

  val incorrectFollowUpUploads =
    followUpUploads.map(
      upload => upload.copy(
        record = upload.record.copy(followUps = None)
      )
    )


  "Validation" must "have succeeded or failed for PatientRecord with inadmissible sequencing types depending on execution date" in { 

    for { 
      outcome <- service ! Validate(nonAdmissibleUploads.next,false)
    } yield outcome must matchPattern { case Left(_: UnacceptableIssuesDetected) => }

  }


  it must "have succeeded for PatientRecord with admissible sequencing types" in { 

    for { 
      outcome <- service ! Validate(admissibleUploads.next,false)
    } yield outcome must matchPattern { case Right(_: DataValid[_]) => }

  }


  it must "have succeeded for PatientRecord with correct FollowUps" in { 

    for { 
      outcome <- service ! Validate(followUpUploads.next,false)
    } yield outcome must matchPattern { case Right(_: DataValid[_]) => }

  }


  it must "have failed for PatientRecord with incorrect FollowUps" in { 

    for { 
      outcome <- service ! Validate(incorrectFollowUpUploads.next,false)
    } yield outcome must matchPattern { case Left(_: UnacceptableIssuesDetected) => }

  }


  "Validation outcomes" must "have been persisted" in {

    val upload = incorrectFollowUpUploads.next

    for { 
      outcome <- service ! Validate(upload,true)

      _ = outcome must matchPattern { case Left(_: UnacceptableIssuesDetected) => }

      validationReport <- service.validationReport(upload.record.id)

      patientRecord <- service.patientRecord(upload.record.id)

      _ = validationReport must be (defined)

      _ = patientRecord must be (defined)

    } yield succeed

  }

}
