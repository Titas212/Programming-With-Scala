import scala.::

object List
{

  trait List[+A]

  case object Nil extends List[Nothing]

  case class ::[+A](head: A, tail: List[A]) extends List[A]

  //3.6
  def init[A](l:List[A]): List[A] = l match
  {
    case Nil => Nil
    case ::(h, Nil) => Nil
    case ::(h, t) => ::(h, init(t))
  }

  def tail[A](as: List[A]): List[A] = as match
  {
    case ::(_, tail) => tail
    case Nil => as
  }

  def drop[A](l: List[A], n: Int): List[A] =
  {
    if (n <= 0) l
    else l match
    {
      case Nil => Nil
      case ::(_, t) => drop(t, n - 1)
    }
  }

  def test[A](l: Array[A], f: A => Boolean): Int =
    {
      def go(n: Int): Int =
      {
          if(n >= l.length) -1
          else if(f(l(n))) n
          else go(n+1)
      }
      go(0)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    {
      l match
      {
        case Nil => Nil
        case ::(h, t) if(f(h)) => dropWhile(t, f)
        case _ => l
      }
    }

  def setHead[A](as: List[A], value: A): List[A] =
  {
    ::(value, tail(as))
  }

//3.8-----------------------------
  def sum(ints: List[Int]): Int =
  {
    foldRight(ints, 0)((x, y) => x+y)
  }

  def product(ds: List[Double]): Double =
  {
    foldRight(ds, 1.0)(_*_)
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
  as match
  {
    case Nil => z
    case ::(x, xs) => f(x, foldRight(xs, z)(f))
  }
//-----------------------------------

//3.9-------------------------------
  def length[A](as: List[A]): Int =
  {
    foldRight(as, 0)((x, acc) => acc+1)
  }
//----------------------------------

  //3.10----------------------------------
  def foldLeft[A, B](as: List[A], z:B)(f:(B,A) => B): B =
  as match
  {
    case Nil => z
    case h::t => foldLeft(t, f(z, h))(f)
  }
  //----------------------------------------

  //3.11 ----------------------------------------
  def sumLeft(ints: List[Int]): Int =
    {
    foldLeft(ints, 0)((x, y) => x+y)
    }

  def productLeft(ds: List[Double]): Double =
    {
    foldLeft(ds, 1.0)(_ * _)
    }

  def lengthLeft[A](as: List[A]): Int =
    {
    foldLeft(as, 0)((acc, _) => acc+1)
    }
  //---------------------------------------------

  //3.12-----------------------------------------
  def reverse[A](as: List[A]): List[A] =
  as match
    {
      case Nil => Nil
      case ::(h, t) =>  foldLeft(as, List[A]())((x, y)=> ::(y, x))
    }
  //---------------------------------------------

  //3.14 -----------------------------------------
  def appendViaLeft[A](as: List[A], ap: List[A]): List[A] =
    {
      ::(as, ap)
    }

  def appendViaRight[A](as: List[A], ap: A): List[A] =
    {
      foldRight()
    }
  //-----------------------------------------------
  def apply[A](as: A*): List[A] =
  {
    if(as.isEmpty) Nil
    else ::(as.head, apply(as.tail: _*))
  }

def main(args: Array[String]): Unit =
  {
    println(appendViaLeft(List(1,2,3), 3))
  }
}
