package de.dnpm.dip.service.mvh


import de.dnpm.dip.model.{
  CarePlan,
  NGSReport,
  PatientRecord
}
import NGSReport.Type._


object extensions
{

  private val ngsTypes = 
    Set(
      Panel,
      Exome,
      GenomeShortRead,
      GenomeLongRead
    )


  // MVH-specific extension methods to PatientRecord
  implicit class PatientRecordExtensions[T <: PatientRecord](val record: T) extends AnyVal
  {

    // Get the MVH board "care plan" as the fisrt by date 
    def mvhCarePlan: Option[CarePlan] = 
      record.getCarePlans.minByOption(_.issuedOn)


    // Get sequencing report(s) performed in context of the MVH
    // as those diagnostic reports with a "true" sequencing type
    // and issued after the MVH inclusion conference
     
    def mvhSequencingReports: List[NGSReport] =
      mvhCarePlan match {
        case Some(cp) if !cp.noSequencingPerformedReason.isDefined =>
          record.ngsReports
            .getOrElse(List.empty)
            .filter(
              report => (report.issuedOn isAfter cp.issuedOn) && (ngsTypes contains report.`type`.code)
            )

        case _ => List.empty
      }

  }

}

