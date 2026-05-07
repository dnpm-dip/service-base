package de.dnpm.dip.service


import cats.Monad
import de.dnpm.dip.coding.Coding
import de.dnpm.dip.model.Site
import play.api.libs.json.{
  Reads,
  Writes
}


object FakeConnector
{
  def apply[F[_]: Monad]: Connector[F,Monad[F]] =
    new FakeConnector[F]
}

private class FakeConnector[F[_]] extends Connector[F,Monad[F]]
{

  override def otherSites: Set[Coding[Site]] =
    Set.empty

  override def submit[T <: PeerToPeerRequest: Writes](
    req: T,
    sites: Set[Coding[Site]] = this.otherSites
  )(
    implicit 
    env: Monad[F],
    fr: Reads[req.ResultType] 
  ): F[Map[Coding[Site],Either[String,req.ResultType]]] =
    env.pure(
      Map.empty
    )

}
