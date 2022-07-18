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
      foldLeft(as, ap)((x, y)=> ::(y,x))
    }

  def appendViaRight[A](as: List[A], ap: List[A]): List[A] =
    {
      foldRight(as, ap)((x, y) => ::(x, y))
    }
  //-----------------------------------------------

  //3.15-------------------------------------------
  def concLists[A](li: List[List[A]]): List[A] =
    {
      foldRight(li, Nil:List[A])(appendViaRight)
    }
  //-----------------------------------------------

  //3.16-------------------------------------------
  def add1(li: List[Int]): List[Int] =
  li match
  {
    case Nil => Nil
    case ::(h, t) => ::(h+1, add1(t))
  }
  //-----------------------------------------------

  //3.17-------------------------------------------
  def dToS(li: List[Double]): List[String] =
    li match
    {
      case Nil => Nil
      case ::(h, t) => ::(h.toString, dToS(t))
    }
  //-----------------------------------------------

  //3.18------------------------------------------
  def map[A, B](as: List[A])(f: A=>B): List[B] =
  as match
  {
    case Nil => Nil
    case ::(h, t) => ::(f(h), map(t)(f))
  }

  def add1Generalized(li: List[Int]): List[Int] =
    {
      map(li)(x => x+1)
    }

  def dToSGeneralized(li: List[Double]): List[String] =
    {
      map(li)(x => x.toString)
    }
  //----------------------------------------------

  //3.19------------------------------------------
  def filter[A](as: List[A])(f:A=> Boolean): List[A] =
  as match
    {
      case Nil => as
      case ::(h, t) if(!f(h)) => filter(t)(f)
      case ::(h, t) => ::(h, filter(t)(f))
    }
  //----------------------------------------------

  //3.20------------------------------------------
  def flatMap[A, B](as: List[A])(f:A=>List[B]): List[B] =
  {
    concLists(map(as)(f))
  }
  //----------------------------------------------

  //3.21------------------------------------------
  def filterViaFlatMap[A](li: List[A])(f:A=> Boolean): List[A] =
    {
      flatMap(li)(x => if(f(x)) List(x) else Nil)
    }
  //----------------------------------------------

  //3.22------------------------------------------
  def addListsAndValues(li: List[Int], as: List[Int]): List[Int] =
  (li, as) match
  {
    case(Nil, _) => Nil
    case(_, Nil) => Nil
    case (::(h, t), ::(h1,t1)) => ::(h+h1, addListsAndValues(t, t1))
  }
  //----------------------------------------------

  //3.23------------------------------------------
  def zipWith[A,B,C](li: List[A], as: List[B])(f:(A, B) => C): List[C] =
    (li, as) match
    {
      case(Nil, _) => Nil
      case(_, Nil) => Nil
      case (::(h, t), ::(h1,t1)) => ::(f(h, h1), zipWith(t, t1)(f))
    }
  //----------------------------------------------

  //3.24------------------------------------------
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    (sup, sub) match
    {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Nil, Nil) => true
      case (::(h, t), ::(h1, t1)) => if(h==h1)hasSubsequence(t, t1) else hasSubsequence(t, sub)
    }


  //----------------------------------------------
  def apply[A](as: A*): List[A] =
  {
    if(as.isEmpty) Nil
    else ::(as.head, apply(as.tail: _*))
  }

def main(args: Array[String]): Unit =
  {
    println(hasSubsequence(List(1,2,3,4,5,6), List(7,8,9)))
  }
}
