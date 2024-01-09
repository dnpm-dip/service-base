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
  Age,
  Diagnosis,
  Site,
  Interval,
  LeftClosedRightOpenInterval
}


/*
 * Custom helper type to represent Map entries,
 * in order to have a more explicit JSON serialization format:
 *
 * Default JSON representation of Map[K,V] is a JsArray of sub-JsArrays,
 * in which the first element is the key and second entry the value
 *
 * With this Entry type the key and value are fields on a JsObject
 * { 
 *   "key": <K>,
 *   "value": <V>
 * }
*/
final case class Entry[+K,+V](
  key: K,
  value: V
)

object Entry 
{
  import play.api.libs.json.{
    Json,
    Writes,
    OWrites
  }

  implicit def writes[K: Writes, V: Writes]: OWrites[Entry[K,V]] =
    Json.writes[Entry[K,V]]
}



trait ReportingOps
{

  import scala.util.chaining._


  type Distribution[+T] =
    Seq[ConceptCount[T]]

  type DistributionsBy[+C,+T] =
    Seq[Entry[C,Distribution[T]]]



  def mean[T: Numeric](ts: Iterable[T]): Double =
    if (ts.nonEmpty)
      Numeric[T].toDouble(ts.sum)/ts.size
    else 
      0.0

/*
  def mean[T: Numeric]: PartialFunction[Iterable[T],Double] = {
    case ts if ts.nonEmpty => Numeric[T].toDouble(ts.sum)/ts.size
  }
*/


  def ageDistribution(
    ages: Seq[Age],
    step: Int = 5
  ): Distribution[Interval[Int]] = {

    import Interval._  // for syntax "x isIn interval"
  
    (
      for {

        // Get minimum age, rounded down to multiple of step size
        min <-
          ages.minOption
            .map(_.value.toInt)
            .map(n => n - (n % step))
      
        // Get maximum age, rounded up to multiple of step size
        max <-
          ages.maxOption
            .map(_.value.toInt)
            .map(n => n + (n % step))

        distribution = 
          LazyList
            .from(min,step)
            .takeWhile(_ + step <= max)
            .map {
              left =>
          
                val range =
                  LeftClosedRightOpenInterval(left, left + step)
          
                ConceptCount(
                  range,
                  ages count (_.value.toInt isIn range)
                )
            }

      } yield distribution
    )
    .getOrElse(Seq.empty)

  }


  def distributionBy[T,U](
    ts: Seq[T]
  )(
    grp: T => U
  ): Distribution[U] =
    ts.groupBy(grp)
      .map {
        case (u,seq) =>
          ConceptCount(
            u,
            seq.size,
            None
          )
      }
      .toSeq
      .sorted


//  def DistributionOf[T](
  def distribution[T](
    ts: Seq[T]
  ): Distribution[T] =
    distributionBy(ts)(identity)


  def distributionsOn[A,C,T](
    records: Seq[A]
  )(
    csOn: A => Seq[C],
    tsOn: A => Seq[T]
  ): DistributionsBy[C,T] =
    records.foldLeft(
      Map.empty[C,Seq[T]]
    ){
      (acc,record) =>

      val cs =
        csOn(record)

      val ts =
        tsOn(record)

      cs.foldLeft(acc){
        (accPr,c) =>
          accPr.updatedWith(c)(
            _.map(_ :++ ts)
             .orElse(Some(ts))
          )
      }

    }
    .map {
      case (c,ts) =>
        Entry(
          c,
          distribution(ts)
        )
    }
    .toSeq



  /*
   * Compute frequency distribution of given codings, optionally in a hierarchical structure,
   * i.e. grouped by their parent coding, with fine-grained component counts.
   * In other words, either a simple a two-level tree with structure. 
   *
   *  coding A:
   *    - count
   *    - child codings A.1, A.2, ...:
   *       - count
   *                 
   */
/*
  def CodingFrequencyDistribution[T: Coding.System](
    codings: Seq[Coding[T]],
    hierarchical: Boolean
  )(
    implicit 
    catalogs: CodeSystemProvider[T,cats.Id,cats.Applicative[cats.Id]]
  ): Seq[ConceptCount[Coding[T]]] = {

   import de.dnpm.dip.util.Completer.syntax._

   val codingGroups =
     codings
       .groupBy(_.code)
       .values

   val conceptCounts =    
     if (hierarchical){
       codingGroups
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
                 .map(_.toCoding)
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

     } else {

       codingGroups
         .map {
           cs =>
             // Treat codings with same code but different version
             // as being conceptually continuous across subsequent versions,
             // i.e. being the same entry at the version maximum (i.e. latest version),
             // to avoid seeming duplication by having
             // e.g. (code,2020), (code,2021), (code,2022) 
             // in different "bins", and instead count them as all being (code,2022)
             val maxCodingByVersion =
               cs.maxBy(_.version.getOrElse(catalogs.latestVersion))(catalogs.versionOrdering)
       
             ConceptCount(
               maxCodingByVersion,
               cs.size,
               None
             )
         }
     }

     conceptCounts
       .toSeq
       .sorted
  }
*/

/*
  def GroupedFrequencyDistribution[T: Coding.System](
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

object ReportingOps extends ReportingOps
