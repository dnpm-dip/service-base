package de.dnpm.dip.service.query


import cats.Monad
import de.dnpm.dip.coding.Coding
import de.dnpm.dip.util.Completer
import de.dnpm.dip.model.PatientRecord
import de.dnpm.dip.model.Site
import de.dnpm.dip.service.{
  Connector,
  PeerToPeerRequest
}
import play.api.libs.json.{
  JsObject,
  JsSuccess,
  Format,
  OFormat,
  Reads,
  Writes
}


final case class DummyQueryCriteria()

object DummyQueryCriteria
{
  implicit val format: OFormat[DummyQueryCriteria] =
    OFormat[DummyQueryCriteria](
      _ => JsSuccess(DummyQueryCriteria()),
      _ => JsObject.empty
    )
}


final case class DummyFilters[T <: PatientRecord]
(
  patient: PatientFilter
)
extends Filters[T]


final class FakeResultSet[T <: PatientRecord]
(
  override val id: Query.Id,
  override val results: Seq[Query.Match[T,DummyQueryCriteria]]
)
extends ResultSet[T,DummyQueryCriteria]
{

  type Filter = DummyFilters[T]

  override implicit def toPredicate[F >: Filter](filter: F): T => Boolean =
    _ => true

  def defaultFilter: Filter =
    DummyFilters(
      PatientFilter.on(patientRecords(_ => true))
    )

}


sealed trait FakeConfig[T <: PatientRecord] extends UseCaseConfig
{
  type PatientRecord = T

  type Criteria = DummyQueryCriteria

  type Results = FakeResultSet[T]
}


final class FakeConnector[F[_]] extends Connector[F,Monad[F]]
{

  override def otherSites = Set.empty

  override def submit[T <: PeerToPeerRequest: Writes](
    req: T,
    sites: Set[Coding[Site]]
  )(
    implicit
    env: Monad[F],
    @annotation.unused fr: Reads[req.ResultType]
  ): F[Map[Coding[Site],Either[String,req.ResultType]]] =
    env.pure(Map.empty)

}


final class FakeQueryService[F[+_],T <: PatientRecord: Format]
(
  override val connector: Connector[F,Monad[F]] = new FakeConnector[F]
)
extends BaseQueryService[F,FakeConfig[T]]
{

  override val preparedQueryDB =
    new InMemPreparedQueryDB[F,Monad,Criteria]

  override val db =
    new InMemLocalDB[F,Monad,Criteria,T](criteria => (t => Some(criteria)))

  override lazy val cache =
    new BaseQueryCache[Criteria,Results,T]


  override implicit val criteriaCompleter: Completer[Criteria] =
    identity[Criteria]

  protected val CriteriaExpander: Completer[Criteria] =
    identity[Criteria]


  protected def ResultSetFrom(
    query: Query[Criteria],
    results: Seq[Query.Match[T,Criteria]]
  ): Results =
    new FakeResultSet(query.id,results)

}


/*
final case class DummyFilters
(
  patient: PatientFilter
)
extends Filters[DummyPatientRecord]


final class FakeResultSet
(
  override val id: Query.Id,
  override val results: Seq[Query.Match[DummyPatientRecord,DummyQueryCriteria]]
)
extends ResultSet[DummyPatientRecord,DummyQueryCriteria]
{

  type Filter = DummyFilters

  override implicit def toPredicate[F >: Filter](filter: F): DummyPatientRecord => Boolean =
    _ => true

  def defaultFilter: Filter =
    DummyFilters(
      PatientFilter.on(patientRecords(_ => true))
    )

}


sealed trait FakeConfig extends UseCaseConfig
{
  type PatientRecord = DummyPatientRecord

  type Criteria = DummyQueryCriteria

  type Results = FakeResultSet
}


final class FakeConnector[F[_]] extends Connector[F,Monad[F]]
{

  override def otherSites = Set.empty

  override def submit[T <: PeerToPeerRequest: Writes](
    req: T,
    sites: Set[Coding[Site]]
  )(
    implicit
    env: Monad[F],
    @annotation.unused fr: Reads[req.ResultType]
  ): F[Map[Coding[Site],Either[String,req.ResultType]]] =
    env.pure(Map.empty)

}


final class FakeQueryService[F[+_]] extends BaseQueryService[F,FakeConfig]
{

  override val preparedQueryDB =
    new InMemPreparedQueryDB[F,Monad,Criteria]

  override val db =
    new InMemLocalDB[F,Monad,Criteria,PatientRecord](criteria => (t => Some(criteria)))

  override val connector =
    new FakeConnector[F]

  override lazy val cache =
    new BaseQueryCache[Criteria,Results,PatientRecord]


  override implicit val criteriaCompleter: Completer[Criteria] =
    identity[Criteria]

  protected val CriteriaExpander: Completer[Criteria] =
    identity[Criteria]


  protected def ResultSetFrom(
    query: Query[Criteria],
    results: Seq[Query.Match[PatientRecord,Criteria]]
  ): Results =
    new FakeResultSet(query.id,results)

}
*/
