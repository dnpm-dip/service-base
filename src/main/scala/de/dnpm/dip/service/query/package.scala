package de.dnpm.dip.service


package object query
{

  type ConceptCount[+T] = Entry[T,Int]


  object ConceptCount
  {

    def apply[T](
      t: T,
      count: Int,
      children: Option[Seq[ConceptCount[T]]] = None
    ): ConceptCount[T] =
      Entry(t,count,children)

  }


}
