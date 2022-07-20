import scala.::

object MyOption
{
  sealed trait MyOption[+A]
  {
    def map[B](f:A=>B):MyOption[B] = this match
      {
      case None => None
      case Some(x) => Some(f(x))
      }

    def flatMap[B](f:A=> MyOption[B]): MyOption[B] =
      map(f).getOrElse(None)

      def getOrElse[B >: A](default: => B): B = this match {
        case None => default
        case Some(x) => x
      }

    def orElse[B>:A] (ob: => MyOption[B]): MyOption[B] =
      {
        map(Some(_)).getOrElse(ob)
      }
      def filter(f: A =>Boolean): MyOption[A] =
        {
          flatMap(x => if(f(x)) Some(x) else None)
        }
  }
  case class Some[+A](get: A) extends MyOption[A]
  case object None extends MyOption[Nothing]

  def map2[A,B,C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] =
  {
    a.flatMap(aa => b.map(bb => f(aa, bb)))
  }
  // a bit later in the chapter we'll learn nicer syntax for
  // writing functions like this
  def map[A,B,C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] =
  {
    for {
      aa <- a
      bb <- b
    }yield f(aa,bb)
  }

  def Try[A](a: => A): MyOption[A] =
    {
      try Some(a)
      catch {case e: Exception => None}
    }

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): MyOption[Double] =
    {
      val optAge: MyOption[Int] = Try (age.toInt)
      val optTickets: MyOption[Int] = Try(numberOfSpeedingTickets.toInt)
      map2(optAge, optTickets)((x, y) => x+y)
    }

  def sequence[A](a: List[MyOption[A]]): MyOption[List[A]] = a match
    {
    case Nil => Some(Nil)
    case ::(h, t) => h.flatMap(x=> sequence(t).map(x::_))
    }

  def traverse[A, B](a: List[A])(f:A=> MyOption[B]): MyOption[List[B]] = a match
    {
    case Nil => Some(Nil)
    case ::(h, t) => map2(f(h), traverse(t)(f))(::(_,_))
    }

  def apply[A](as: A*): MyOption[A] =
  {
    if(as.isEmpty) None
    else Some(as(0))
  }
  def main(args:Array[String]): Unit = {
    println(MyOption(5).flatMap(x =>Some(x+5)))
  }
}
