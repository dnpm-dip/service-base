package de.dnpm.dip.service.validation


//import java.time.LocalDate
import java.time.LocalDate.{now => today}
import java.time.format.DateTimeFormatter.ISO_LOCAL_DATE
//import java.time.Month.MAY
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


class ValidationServiceTests extends AsyncFlatSpec with Matchers
// with Validators
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


  "Validation" must "have succeeded for PatientRecord with inadmissible sequencing types but future enforcement date" in { 

    System.setProperty("dnpm.dip.extended.qc.enforcement.date", ISO_LOCAL_DATE.format(today plusWeeks 1))

    for { 
      outcome <- service ! Validate(nonAdmissibleUploads.next)
    } yield outcome must matchPattern { case Right(_: DataValid[_]) => }

  }


  it must "have failed for PatientRecord with inadmissible sequencing types after enforcement date" in { 

    System.setProperty("dnpm.dip.extended.qc.enforcement.date", ISO_LOCAL_DATE.format(today minusWeeks 1))

    for { 
      outcome <- service ! Validate(nonAdmissibleUploads.next)
    } yield outcome must matchPattern { case Left(_: UnacceptableIssuesDetected) => }

  }


  it must "have succeeded for PatientRecord with admissible sequencing types" in { 

    for { 
      outcome <- service ! Validate(admissibleUploads.next)
    } yield outcome must matchPattern { case Right(_: DataValid[_]) => }

  }

  it must "have succeeded for PatientRecord with correct FollowUps" in { 

    for { 
      outcome <- service ! Validate(followUpUploads.next)
    } yield outcome must matchPattern { case Right(_: DataValid[_]) => }

  }

  it must "have failed for PatientRecord with incorrect FollowUps" in { 

    for { 
      outcome <- service ! Validate(incorrectFollowUpUploads.next)
    } yield outcome must matchPattern { case Left(_: UnacceptableIssuesDetected) => }

  }

}
