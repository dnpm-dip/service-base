package de.dnpm.dip.service.query



import play.api.libs.json.{
  Json,
  Writes,
  OWrites
}


trait Collection[T]
{

  def total: Int

  def size: Int

  def offset: Option[Int]

  def limit: Option[Int]

  def appliedFilter: Option[T => Boolean]

  def entries: Seq[T]

  def map[U](
    f: T => U
  )(
    implicit
    filter: Option[U => Boolean]
  ): Collection[U]

  def withOffset(n: Int): Collection[T]

  def withLimit(n: Int): Collection[T]

  def withFilter(f: T => Boolean): Collection[T]

}


object Collection
{
  
  import scala.util.chaining._

  private case class Impl[T]
  (
    private val seq: Seq[T],
    appliedFilter: Option[T => Boolean] = None,
    offset: Option[Int] = None ,
    limit: Option[Int] = None,
  )
  extends Collection[T]
  {

    override val total =
      seq.size

    override def entries =
      seq
        .pipe(
          ts => appliedFilter.fold(ts)(ts.filter)
        )
        .pipe(
          ts => offset.fold(ts)(ts.drop)
        )
        .pipe(
          ts => limit.fold(ts)(ts.take)
        )

    override def size =
      entries.size

    override def map[U](
      f: T => U
    )(
      implicit
      filter: Option[U => Boolean]
    ): Collection[U] =
      Impl(
        seq map f,
        filter,
        offset,
        limit
      )

    override def withOffset(n: Int): Collection[T] =
      this.copy(offset = Some(n))
      
    override def withLimit(n: Int): Collection[T] =
      this.copy(limit = Some(n))

    override def withFilter(f: T => Boolean): Collection[T] =
      this.copy(appliedFilter = Some(f))
  }


  def apply[T](
    seq: Seq[T],
    filter: Option[T => Boolean] = None,
    offset: Option[Int] = None,
    limit: Option[Int] = None
  ): Collection[T] =
    Impl(
      seq,
      filter,
      offset,
      limit
    )


  implicit def writesCollection[T: Writes]: OWrites[Collection[T]] =
    OWrites {
      coll =>
        Json.obj(
          "total"   -> Json.toJson(coll.total),
          "size"    -> Json.toJson(coll.size),
          "entries" -> Json.toJson(coll.entries),
        )
        .pipe(
          js => 
            coll.offset.fold(js)(n => js + ("offset" -> Json.toJson(n)))
        )
        .pipe(
          js => 
            coll.limit.fold(js)(n => js + ("limit" -> Json.toJson(n)))
        )
    }

}
