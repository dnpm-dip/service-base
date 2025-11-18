package de.dnpm.dip.service



trait Deidentifier[T,Ctx]
{
  def apply(t: T)(implicit ctx: Ctx): T
}

object Deidentifier
{
  def apply[T,Ctx](implicit deidentifier: Deidentifier[T,Ctx]) = deidentifier

  implicit def fromFunction[T](f: T => T): Deidentifier[T,DummyImplicit] =
    new Deidentifier[T,DummyImplicit]{
      override def apply(t: T)(implicit ctx: DummyImplicit) = f(t)
    }

  implicit def fromFunction[T,Ctx](f: (T,Ctx) => T): Deidentifier[T,Ctx] =
    new Deidentifier[T,Ctx]{
      override def apply(t: T)(implicit ctx: Ctx) = f(t,ctx)
    }

  def apply[T,Ctx](f: (T,Ctx) => T): Deidentifier[T,Ctx] =
    new Deidentifier[T,Ctx]{
      override def apply(t: T)(implicit ctx: Ctx) = f(t,ctx)
    }
}

