object MyEither
{
  sealed trait MyEither[+E, +A]
  {
    def mean(xs: IndexedSeq[Double]): MyEither[String, Double] =
    {
      if(xs.isEmpty)
        Left("mean of empty list!")
      else
        Right(xs.sum/xs.length)
    }

    def map[B](f:A=>B):MyEither[E,B] = this match
      {
        case Right(x) => Right(f(x))
        case Left(e) => Left(e)
      }

    def flatMap[EE>: E,B](f: A => MyEither[EE,B]): MyEither[EE, B] = this match
      {
      case Left(x) => Left(x)
      case Right(x) => f(x)
      }

    def orElse[EE>: E,B >: A](b: => MyEither[EE, B]): MyEither[EE, B] = this match
      {
        case Left(x) => b
        case Right(x) => Right(x)
      }

    def map2[EE>: E, B, C](b: MyEither[EE, B])(f: (A, B) => C): MyEither[EE, C] =
      {
        for
        {
          a <- this
          bb <- b
        } yield(f(a, bb))
      }
  }
  case class Left[+E](value: E) extends MyEither[E, Nothing]
  case class Right[+A](value: A) extends MyEither[Nothing, A]

  def sequence[E, A](es: List[MyEither[E, A]]): MyEither[E, List[A]] =
  {
    traverse(es)(x => x)
  }

  def traverse[E, A, B](as: List[A])(f: A => MyEither[E, B]): MyEither[E, List[B]] = as match
    {
      case Nil => Right(Nil)
      case h::t => f(h).flatMap(x => traverse(t)(f).map(x:: _))
    }

  def Try[A](a: => A): MyEither[Exception, A] =
    try Right(a)
    catch {case e: Exception => Left(e)}

  def main(args: Array[String]): Unit =
    {

    }
}
