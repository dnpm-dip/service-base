package de.dnpm.dip.service.query


import scala.util.Either
import cats.Functor
import cats.data.IorNel
import de.dnpm.dip.coding.{
  Code,
  Coding
}
import de.dnpm.dip.model.{
  Id,
  Patient,
  Snapshot,
  Site
}


trait Connector[
  F[_],
  Env,
//  Response
]{
  self =>

  def localSite: Coding[Site]

  def otherSites: List[Coding[Site]]


  def !(
    req: PeerToPeerRequest,
    sites: List[Coding[Site]] = self.otherSites
  )(
    implicit env: Env
  ): F[Map[Coding[Site],Either[String,req.ResultType]]]


  def !(
    req: PeerToPeerRequest,
    site: Coding[Site]
  )(
    implicit
    env: Env,
    app: Functor[F]
  ): F[Either[String,req.ResultType]] = {

    import cats.syntax.functor._
    import cats.syntax.either._

    (self ! (req,List(site)))
      .map(_.head._2)

  }

}
