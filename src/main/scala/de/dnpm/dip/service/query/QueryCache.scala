package de.dnpm.dip.service.query


import scala.util.Either
import cats.data.IorNel
import de.dnpm.dip.util.Logging
import de.dnpm.dip.model.Snapshot



trait QueryCache[
  Criteria,
  Filters <: Query.Filters,
  Results,
  PatientRecord
]{
  self =>

  def timeoutSeconds: Int

  def newQueryId: Query.Id

  def add(
    queryWithResults: (Query[Criteria,Filters],Results)
  ): Unit

  final def += (
    queryWithResults: (Query[Criteria,Filters],Results)
  ) = add(queryWithResults)

  def queries: Seq[Query[Criteria,Filters]]

  def get(id: Query.Id): Option[(Query[Criteria,Filters],Results)]

  def getQuery(id: Query.Id): Option[Query[Criteria,Filters]] =
    self.get(id).map(_._1)

  def getResults(id: Query.Id): Option[Results] =
    self.get(id).map(_._2)
    
  def remove(id: Query.Id): Unit

  final def -= (id: Query.Id): Unit =
    self.remove(id)

}



class BaseQueryCache[
  Criteria,
  Filters <: Query.Filters,
  Results,
  PatientRecord
]
extends QueryCache[
  Criteria,
  Filters,
  Results,
  PatientRecord
]
with Logging
{

  import java.util.UUID.randomUUID
  import java.time.Instant.now
  import java.time.temporal.ChronoUnit.MINUTES
  import java.util.concurrent.Executors
  import java.util.concurrent.TimeUnit.SECONDS
  import scala.collection.concurrent._


  private val cache: Map[Query.Id,(Query[Criteria,Filters],Results)] =
    TrieMap.empty



  private val executor =
    Executors.newSingleThreadScheduledExecutor

  // Amount of inactivity time after which a query is removed
  private val timeOut =
    System.getProperty(
      "dnpm.dip.query.cache.timeout.minutes",
      15.toString  // 15 min default
    )
    .toInt 

  override val timeoutSeconds =
    timeOut * 60


  private val cleanUpPeriod =
    System.getProperty(
      "dnpm.dip.query.cache.cleanup.period.seconds",
      60.toString
    )
    .toInt


  executor.scheduleAtFixedRate(
    () => {

      log.debug("Running clean-up task for timed out Query sessions")

      for {
        (query,_) <- cache.values
        if (query.lastUpdate isBefore now.minus(timeOut,MINUTES))
      }{
        log.info(s"Removing timed out Query ${query.id.value}")
        remove(query.id)
      }

    },
    cleanUpPeriod, // delay  
    cleanUpPeriod, // period
    SECONDS
  )


  private val touch: Query[Criteria,Filters] => Query[Criteria,Filters] =
    _.copy(lastUpdate = now)


  override def newQueryId: Query.Id =
    Query.Id(randomUUID.toString)
  

  override def add(
    queryWithResults: (Query[Criteria,Filters],Results)
  ): Unit =
    cache += (queryWithResults._1.id -> queryWithResults)
  

  override def queries: Seq[Query[Criteria,Filters]] =
    cache.values
      .map(_._1)
      .toSeq


  override def get(id: Query.Id): Option[(Query[Criteria,Filters],Results)] = 
    cache.updateWith(id){
      case Some(query -> results) => Some(touch(query) -> results)
      case _          => None
    }


  override def remove(id: Query.Id): Unit =
    cache -= id

}

  
/*
trait QueryCache[
  Criteria,
  Filters <: Query.Filters,
  Results,
  PatientRecord
]{
  self =>

  type Cohort = Seq[Snapshot[PatientRecord]]

  def timeoutSeconds: Int

  def newQueryId: Query.Id

  def add(
    queryWithResults: (Query[Criteria,Filters],Results,Cohort)
  ): Unit

  final def += (
    queryWithResults: (Query[Criteria,Filters],Results,Cohort)
  ) = add(queryWithResults)


  def get(id: Query.Id): Option[(Query[Criteria,Filters],Results,Cohort)]

  def getQuery(id: Query.Id): Option[Query[Criteria,Filters]] =
    self.get(id).map(_._1)

  def getResults(id: Query.Id): Option[Results] =
    self.get(id).map(_._2)
    
  def getCohort(id: Query.Id): Option[Cohort] =
    self.get(id).map(_._3)
    

  def remove(id: Query.Id): Unit

  final def -= (id: Query.Id): Unit =
    self.remove(id)

}



class BasicQueryCache[
  Criteria,
  Filters <: Query.Filters,
  Results,
  PatientRecord
]
extends QueryCache[
  Criteria,
  Filters,
  Results,
  PatientRecord
]
with Logging
{

  import java.util.UUID.randomUUID
  import java.time.Instant.now
  import java.time.temporal.ChronoUnit.MINUTES
  import java.util.concurrent.Executors
  import java.util.concurrent.TimeUnit.SECONDS
  import scala.collection.concurrent._


  private val cache: Map[Query.Id,(Query[Criteria,Filters],Results,Cohort)] =
    TrieMap.empty



  private val executor =
    Executors.newSingleThreadScheduledExecutor

  // Amount of inactivity time after which a query is removed
  private val timeOut =
    System.getProperty(
      "dnpm.dip.query.cache.timeout.minutes",
      15.toString  // 15 min default
    )
    .toInt 

  override val timeoutSeconds =
    timeOut * 60


  private val cleanUpPeriod =
    System.getProperty(
      "dnpm.dip.query.cache.cleanup.period.seconds",
      60.toString
    )
    .toInt


  executor.scheduleAtFixedRate(
    () => {

      log.debug("Running clean-up task for timed out Query sessions")

      for {
        (query,_,_) <- cache.values
        if (query.lastUpdate isBefore now.minus(timeOut,MINUTES))
      }{
        log.info(s"Removing timed out Query ${query.id.value}")
        remove(query.id)
      }

    },
    cleanUpPeriod, // delay  
    cleanUpPeriod, // period
    SECONDS
  )


  private val touch: Query[Criteria,Filters] => Query[Criteria,Filters] =
    _.copy(lastUpdate = now)


  override def newQueryId: Query.Id =
    Query.Id(randomUUID.toString)
  

  override def add(
    queryWithResults: (Query[Criteria,Filters],Results,Cohort)
  ): Unit =
    cache += (queryWithResults._1.id -> queryWithResults)
  

  override def get(id: Query.Id): Option[(Query[Criteria,Filters],Results,Cohort)] = 
    cache.updateWith(id){
      case Some(tup3) => Some(tup3.copy(_1 = touch(tup3._1)))
      case _          => None
    }
//    cache.get(id)


  override def remove(id: Query.Id): Unit =
    cache -= id
  

}
*/
