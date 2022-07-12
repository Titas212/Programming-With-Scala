object List
{

  trait List[+A]

  case object Nil extends List[Nothing]

  case class ::[+A](head: A, tail: List[A]) extends List[A]

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


  def apply[A](as: A*): List[A] =
  {
    if(as.isEmpty) Nil
    else ::(as.head, apply(as.tail: _*))
  }
}
