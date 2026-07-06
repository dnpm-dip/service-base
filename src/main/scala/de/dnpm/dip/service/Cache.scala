package de.dnpm.dip.service


import java.time.Instant
import java.util.concurrent.Executors
import java.util.concurrent.ScheduledExecutorService
import java.util.concurrent.TimeUnit.MILLISECONDS
import scala.collection.concurrent.TrieMap
import scala.concurrent.duration._
import scala.reflect.ClassTag


/**
 * Simple/light-weight Cache utility, inspired from:
 * https://www.playframework.com/documentation/3.0.x/api/scala/play/api/cache/SyncCacheApi.html
 * scala.collection.mutable.Map
 */

import Cache.Expiration.Never

trait Cache[K,V] extends AutoCloseable
{

  def filter(f: (K,V) => Boolean): Map[K,V]

  def filterAs[T <: V: ClassTag](f: (K,V) => Boolean): Map[K,T] =
    filter(f).collect { case (k,t: T) => (k,t) }

  /**
   * Get the value associated with `key`, thereby also updating the last access timestamp
   */
  def get(key: K): Option[V]

  /**
   * Get the value associated with `key` if it is an instance of `T`.
   *
   * Note that this check relies on a `ClassTag` and therefore is subject to
   * JVM type erasure. For example, `List[Int]` and `List[String]` cannot be
   * distinguished at runtime.
   */
  def getAs[T <: V: ClassTag](key: K): Option[T] =
    get(key).collect { case t: T => t }

  def getOrElseUpdate(key: K, value: => V, expiration: Cache.Expiration = Never): V

  def put(key: K, value: V, expiration: Cache.Expiration = Never): Unit

  def remove(key: K): Option[V]

}

object Cache
{

  sealed trait Expiration
  {
    def isExpired(lastAccess: Instant, now: Instant): Boolean
  }

  object Expiration
  {
    final case class Sliding(ttl: FiniteDuration) extends Expiration
    {
      override def isExpired(lastAccess: Instant, now: Instant) =
        lastAccess.plusMillis(ttl.toMillis) isBefore now
    }
    
    final case class At(instant: Instant) extends Expiration
    {
      override def isExpired(lastAccess: Instant, now: Instant) =
        now isAfter instant
    }

    case object Never extends Expiration
    {
      override def isExpired(lastAccess: Instant, now: Instant) =
        false
    }

    implicit def fromFiniteDuration(d: FiniteDuration): Expiration =
      Sliding(d)

    implicit def fromInstant(t: Instant): Expiration =
      At(t)

    implicit def fromInfDuration(inf: Duration.Infinite): Expiration =
      Never
  }

  private final case class Entry[V]
  (
    value: V,
    expiration: Expiration,
    lastAccess: Instant
  )

  private final class Impl[K,V]
  (
    executor: ScheduledExecutorService,
    ownsExecutor: Boolean,
    period: FiniteDuration
  )
  extends Cache[K,V]
  { 

    private val entries: scala.collection.concurrent.Map[K,Entry[V]] = TrieMap.empty

    private val cleanupTask = 
      executor.scheduleAtFixedRate(
        () => entries.filterInPlace {
          case (_,Entry(_,expiration,t)) => !expiration.isExpired(t,Instant.now)
        },
        period.toMillis,
        period.toMillis,
        MILLISECONDS
      )


    override def filter(f: (K,V) => Boolean): Map[K,V] =
      entries.collect {
        case (key,entry) if f(key,entry.value) => 
          entries.update(key,entry.copy(lastAccess = Instant.now))
          (key,entry.value)
      }
      .toMap


    override def get(key: K): Option[V] = {

      val now = Instant.now

      entries.updateWith(key){
        case Some(entry) if !entry.expiration.isExpired(entry.lastAccess,now) => Some(entry.copy(lastAccess = now))
        case _ => None
      }
      .map(_.value)
    }


    override def getOrElseUpdate(key: K, value: => V, expiration: Expiration = Never): V =
      this.get(key).getOrElse {
        val computed = value // Compute thunk only once
        entries.putIfAbsent(key,Entry(computed,expiration,Instant.now)) match { 
          case Some(entry) => entry.value
          case None        => computed
        }
      }


    override def remove(key: K): Option[V] =
      entries.remove(key).map(_.value)


    override def put(key: K, value: V, expiration: Expiration = Never): Unit = {
      entries += key -> Entry(value,expiration,Instant.now)
      ()
    }

    override def close() = {

      cleanupTask.cancel(false)

      if (ownsExecutor) executor.shutdown
    }

  }


  def apply[K,V](
    executor: ScheduledExecutorService,
    cleanupPeriod: FiniteDuration = 60 seconds 
  ): Cache[K,V] =
    new Impl(executor,false,cleanupPeriod)

  def empty[K,V](
    cleanupPeriod: FiniteDuration = 60 seconds 
  ): Cache[K,V] =
    new Impl(Executors.newSingleThreadScheduledExecutor,true,cleanupPeriod)

}

