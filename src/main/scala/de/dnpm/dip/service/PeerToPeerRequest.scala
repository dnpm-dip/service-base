package de.dnpm.dip.service


import de.dnpm.dip.coding.Coding
import de.dnpm.dip.model.Site
import play.api.libs.json.{
  Json,
  Format,
  Writes,
  Reads
}


trait PeerToPeerRequest
{
  type ResultType

  val origin: Coding[Site]
}

