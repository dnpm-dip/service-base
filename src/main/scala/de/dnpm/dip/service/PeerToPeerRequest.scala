package de.dnpm.dip.service


import de.dnpm.dip.coding.Coding
import de.dnpm.dip.model.Site


trait PeerToPeerRequest
{
  type ResultType

  val origin: Coding[Site]
}

