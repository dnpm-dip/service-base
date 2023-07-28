package de.dnpm.dip.service.query


import scala.util.Either
import cats.data.IorNel
import de.dnpm.dip.util.Logging
import de.dnpm.dip.model.Snapshot


trait QueryCache[
  Parameters,
  Filters <: Query.Filters,
  Results,
  PatientRecord
]{
  self =>

  type Cohort = Seq[Snapshot[PatientRecord]]


  def newQueryId: Query.Id

  def add(
    queryWithResults: (Query[Parameters,Filters],Results,Cohort)
  ): Unit

  final def +=(
    queryWithResults: (Query[Parameters,Filters],Results,Cohort)
  ) = add(queryWithResults)


  def get(id: Query.Id): Option[(Query[Parameters,Filters],Results,Cohort)]

  def getQuery(id: Query.Id): Option[Query[Parameters,Filters]] =
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
  Parameters,
  Filters <: Query.Filters,
  Results,
  PatientRecord
]
extends QueryCache[
  Parameters,
  Filters,
  Results,
  PatientRecord
]
with Logging
{

  import java.util.UUID.randomUUID
  import java.time.Instant
  import java.time.temporal.ChronoUnit.MINUTES
  import java.util.concurrent.Executors
  import java.util.concurrent.TimeUnit.SECONDS
  import scala.collection.concurrent._


  private val cache: Map[Query.Id,(Query[Parameters,Filters],Results,Cohort)] =
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
        if (query.lastUpdate isBefore Instant.now.minus(timeOut,MINUTES))
      }{
        log.info(s"Removing timed out Query ${query.id.value}")
        remove(query.id)
      }

    },
    cleanUpPeriod, // delay  
    cleanUpPeriod, // period
    SECONDS
  )


  override def newQueryId: Query.Id = {
    Query.Id(randomUUID.toString)
  }

  override def add(
    queryWithResults: (Query[Parameters,Filters],Results,Cohort)
  ): Unit = {

    cache += (queryWithResults._1.id -> queryWithResults)

  }

  override def get(id: Query.Id): Option[(Query[Parameters,Filters],Results,Cohort)] = {
    cache.get(id)
  }

  override def remove(id: Query.Id): Unit = {
    cache -= id
  }

}



/*
trait QueryCache[
  Parameters,
  Filters <: Query.Filters,
  Results
]{
  self =>

  def newQueryId: Query.Id

  def add(
    queryWithResults: (Query[Parameters,Filters],Results)
  ): Unit

  final def +=(
    queryWithResults: (Query[Parameters,Filters],Results)
  ) = add(queryWithResults)


  def get(id: Query.Id): Option[(Query[Parameters,Filters],Results)]

  def getQuery(id: Query.Id): Option[Query[Parameters,Filters]] =
    self.get(id).map(_._1)

  def getResults(id: Query.Id): Option[Results] =
    self.get(id).map(_._2)
    

  def remove(id: Query.Id): Unit

  final def -= (id: Query.Id): Unit =
    self.remove(id)

}


class BasicQueryCache[
  Parameters,
  Filters <: Query.Filters,
  Results
]
extends QueryCache[
  Parameters,
  Filters,
  Results
]
with Logging
{

  import java.util.UUID.randomUUID
  import java.time.Instant
  import java.time.temporal.ChronoUnit.MINUTES
  import java.util.concurrent.Executors
  import java.util.concurrent.TimeUnit.SECONDS
  import scala.collection.concurrent._


  private val cache: Map[Query.Id,(Query[Parameters,Filters],Results)] =
    TrieMap.empty



  private val executor =
    Executors.newSingleThreadScheduledExecutor

  // Amount of inactivity time after which a query is removed
  private val timeOut =
    System.getProperty(
      "dppm.query.cache.timeout.minutes",
      15.toString  // 15 min default
    ).toInt 

  private val cleanUpPeriod =
    System.getProperty(
      "dppm.query.cache.cleanup.period.seconds",
      60.toString
    ).toInt


  executor.scheduleAtFixedRate(
    () => {

      log.debug("Running clean-up task for timed out Query sessions")

      for {
        (query,_) <- cache.values
        if (query.lastUpdate isBefore Instant.now.minus(timeOut,MINUTES))
      }{
        log.info(s"Removing timed out Query ${query.id.value}")
        remove(query.id)
      }

    },
    cleanUpPeriod, // delay  
    cleanUpPeriod, // period
    SECONDS
  )


  override def newQueryId: Query.Id = {
    Query.Id(randomUUID.toString)
  }

  override def add(
    queryWithResults: (Query[Parameters,Filters],Results)
  ): Unit = {

    cache += (queryWithResults._1.id -> queryWithResults)

  }

  override def get(id: Query.Id): Option[(Query[Parameters,Filters],Results)] = {
    cache.get(id)
  }

  override def remove(id: Query.Id): Unit = {
    cache -= id
  }

}
*/
