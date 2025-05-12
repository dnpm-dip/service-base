package de.dnpm.dip

package object service
{

type ConceptCount[+T] = Entry[T,Count]

type DistributionsBy[K,T] = Seq[Entry[K,Distribution[T]]]

}
