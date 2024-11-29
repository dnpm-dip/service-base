package de.dnpm.dip.service

package object query
{


type ConceptCount[+T] = Entry[T,Count]


type DistributionsBy[K,T] = Seq[Entry[K,Distribution[T]]]

}
