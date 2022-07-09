object List
{
  trait List[+A]
  case object  Nil extends List[Nothing]
  case class ::[+A](head: A, tail: List[A]) extends List[A]

  def tail[A](as: List[A]): List[A] = as match
  {
    case ::(_, tail) => tail
    case Nil => as
  }

  def setHead[A](as: List[A], value: A): List[A] =
  {
    ::(value, tail(as))
  }

  def removeFirst[A](as: List[A]): List[A] =
    {
      as
    }

  def apply[A](as: A*): List[A] =
  {
    if(as.isEmpty) Nil
    else ::(as.head, apply(as.tail: _*))
  }
}
