package de.dnpm.dip.service.mvh


import de.dnpm.dip.model.{
  CarePlan,
  EpisodeOfCare,
  NGSReport,
  PatientRecord
}
import NGSReport.Type._


object extensions
{

  private val ngsType = 
    Set(
      Panel,
      Exome,
      GenomeShortRead,
      GenomeLongRead
    )


  // MVH-specific extension methods to PatientRecord
  implicit class PatientRecordExtensions[T <: PatientRecord](val record: T) extends AnyVal
  {

    def currentEpisodeOfCare: EpisodeOfCare =
      record.episodesOfCare.toList.maxBy(_.period.start)


    // Get the indication board CarePlan as the latest with this boardType (if defined),
    // else, by default, get the indication board CarePlan as the first by date 
    def indicationCarePlan: Option[CarePlan] = 
      record.getCarePlans
        .filter(_.boardType.exists(_.code.enumValue == CarePlan.BoardType.IndicationBoard))
        .maxByOption(_.issuedOn)
        .orElse(
          record.getCarePlans.minByOption(_.issuedOn)
        )


    // Get sequencing report(s) performed in context of the MVH
    // as those diagnostic reports with a "true" sequencing type
    // and issued after the indication board
    def mvhSequencingReports: List[NGSReport] =
      indicationCarePlan match {
        case Some(cp) if !cp.noSequencingPerformedReason.isDefined =>
          record.ngsReports
            .getOrElse(List.empty)
            .filter(
              report => (report.issuedOn isAfter cp.issuedOn) && ngsType(report.`type`.code)
            )

        case _ => List.empty
      }

  }

}

