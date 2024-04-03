package de.dnpm.dip.service

package object validation
{

import de.dnpm.dip.model.Id


type HasIdOf[T,V >: T] = T <:< { def id: Id[V] }
type HasId[T]          = HasIdOf[T,_]
type HasSelfId[T]      = HasIdOf[T,T]


}
