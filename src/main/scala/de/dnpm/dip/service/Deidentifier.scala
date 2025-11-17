package de.dnpm.dip.service



trait Deidentifier[T] extends (T => T)


object Deidentifier
{
  def apply[T](implicit deidentifier: Deidentifier[T]) = deidentifier

  implicit def fromFunction[T](f: T => T): Deidentifier[T] =
    new Deidentifier[T]{
      override def apply(t: T) = f(t)
    }
}

