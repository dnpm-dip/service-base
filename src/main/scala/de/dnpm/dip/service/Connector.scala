package de.dnpm.dip.service


import scala.util.Either
import cats.Functor
import cats.data.IorNel
import de.dnpm.dip.coding.{
  Code,
  Coding
}
import de.dnpm.dip.model.Site
import play.api.libs.json.{
  Reads,
  Writes
}


trait Connector[
  F[_],
  Env,
]{
  self =>

  def otherSites: Set[Coding[Site]]


  def submit[T <: PeerToPeerRequest: Writes](
    req: T,
    sites: Set[Coding[Site]] = self.otherSites
  )(
    implicit
    env: Env,
    fr: Reads[req.ResultType] 
  ): F[Map[Coding[Site],Either[String,req.ResultType]]]

  def submit[T <: PeerToPeerRequest: Writes](
    req: T,
    site: Coding[Site]
  )(
    implicit
    env: Env,
    app: Functor[F],
    fr: Reads[req.ResultType] 
  ): F[Either[String,req.ResultType]] = {

    import cats.syntax.functor._
    import cats.syntax.either._

    (self submit (req,Set(site)))
      .map(_.head._2)

  }


  final def ![T <: PeerToPeerRequest: Writes](
    req: T,
    sites: Set[Coding[Site]] = self.otherSites
  )(
    implicit
    env: Env,
    fr: Reads[req.ResultType] 
  ): F[Map[Coding[Site],Either[String,req.ResultType]]] =
    self submit (req,sites)


  final def ![T <: PeerToPeerRequest: Writes](
    req: T,
    site: Coding[Site]
  )(
    implicit
    env: Env,
    app: Functor[F],
    fr: Reads[req.ResultType] 
  ): F[Either[String,req.ResultType]] = 
    self submit (req,site)


}
