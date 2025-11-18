package de.dnpm.dip.service



trait Deidentifier[T,Ctx]
{
  def apply(t: T)(implicit ctx: Ctx): T
}

object Deidentifier
{

  type Of[T] = Deidentifier[T,DummyImplicit] 


  def of[T](implicit deidentifier: Deidentifier.Of[T]) = deidentifier


  def apply[T,Ctx](implicit deidentifier: Deidentifier[T,Ctx]) = deidentifier


  implicit def fromFunction[T](f: T => T): Deidentifier.Of[T] =
    new Deidentifier.Of[T]{
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



  object syntax
  {

    implicit class DeidentificationOps[T](val t: T) extends AnyVal
    {
      def deidentified(implicit deidentifier: Deidentifier.Of[T]) =
        deidentifier(t)

      def deidentifiedWith[Ctx](ctx: Ctx)(implicit deidentifier: Deidentifier[T,Ctx]) =
        deidentifier(t)(ctx)
    }

  }

}

