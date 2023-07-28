package de.dnpm.dip.service.query


import java.time.LocalDateTime
import de.dnpm.dip.coding.{
  Coding,
  CodeSystemProvider
}
import de.dnpm.dip.coding.icd.{
  ICD10GM
}
import de.dnpm.dip.model.{
  Diagnosis,
  Site
}



trait ReportingOps
{

  def ConceptDistribution[T: Coding.System](
    codings: Seq[Coding[T]],
  )(
    implicit 
    catalogs: CodeSystemProvider[T,cats.Id,cats.Applicative[cats.Id]]
  ): Seq[ConceptCount[Coding[T]]] = {

   import de.dnpm.dip.util.Completer.syntax._

   codings
     .groupBy(_.code)
     .values
     .map {
       cs =>
         // Treat codings with same code but different version
         // as being conceptually continuous across subsequent versions,
         // i.e. being the same entry at the version maximum (i.e. latest version),
         // to avoid seeming duplication by having
         // e.g. (code,2020), (code,2021), (code,2022) 
         // in different "bins", and instead count them as all being (code,2022)
         val maxByVersion =
           cs.maxBy(_.version.getOrElse(catalogs.latestVersion))(catalogs.versionOrdering)

         val codeSystem =
           maxByVersion.version.flatMap(catalogs.get).getOrElse(catalogs.latest)

         val coding =
           maxByVersion.complete

         val parent =
           codeSystem
             .parentOf(coding.code)
             .map(Coding.fromConcept(_))
             .getOrElse(coding)

         parent -> (coding -> cs.size)
     }
     // group by parent, mapping the values to categories with their occurrence
     .groupMap(_._1)(_._2)
     .map {
       case (superClass,categoriesWithCount) =>
         ConceptCount(
           superClass,
           categoriesWithCount.map(_._2).sum,
           Some(
             categoriesWithCount.map {
               case (category,n) =>
                 ConceptCount(
                   category,
                   n,
                   None
                 )
             }
             .toSeq
             .sorted
           )
         )
     }
     .toSeq
     .sorted
  }


/*
  def DiagnosisCodeDistribution(
    diagnoses: Seq[Diagnosis],
  )(
    implicit 
    icd10Catalogs: CodeSystemProvider[ICD10GM,cats.Id,cats.Applicative[cats.Id]]
  ): Seq[ConceptCount[Coding[ICD10GM]]] = {

    import ICD10GM.extensions._
    import de.dnpm.dip.util.Completer.syntax._
    
    diagnoses
      .map(_.code)
      .groupBy(_.code)
      .values
      .map {
        codings =>
          // Treat ICD-10 codings with same code but different version
          // as being conceptually continuous across subsequent versions,
          // i.e. being the same entry at the version maximum (i.e. latest version),
          // to avoid seeming duplication by having
          // e.g. ICD-10 (code,2020), (code,2021), (code,2022) 
          // in different "bins", and instead count them as all being (code,2022)
          val maxByVersion =
            codings.maxBy(_.version.getOrElse(icd10Catalogs.latestVersion))(icd10Catalogs.versionOrdering)
    
          val category   =
            maxByVersion.complete

          val superClass =
            maxByVersion.superClass.getOrElse(category)
    
          superClass -> (category -> codings.size)
      }
      // group by ICD10 superclass, mapping the values to ICD categories with their occurrence
      .groupMap(_._1)(_._2)
      .map {
        case (superClass,categoriesWithCount) =>
          ConceptCount(
            superClass,
            categoriesWithCount.map(_._2).sum,
            Some(
              categoriesWithCount.map {
                case (category,n) =>
                  ConceptCount(
                    category,
                    n,
                    None
                  )
              }
              .toSeq
              .sorted
            )
          )
      }
      .toSeq
      .sorted
  }
*/

}
