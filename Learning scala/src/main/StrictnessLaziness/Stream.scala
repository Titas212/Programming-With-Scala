object Stream {

  sealed trait Stream[+A] {
    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }

    def toList: List[A] = this match {
      case Empty => Nil
      case Cons(h, t) => ::(h(), t().toList)
    }

    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if (n > 1) => cons(h(), t().take(n - 1))
      case Cons(h, _) if (n == 1) => cons(h(), Empty)
      case _ => empty
    }

    def drop(n: Int): Stream[A] = this match {
      case Cons(h, t) if (n == 0) => cons(h(), t())
      case Cons(h, t) => t().drop(n - 1)
    }

    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if (p(h())) => cons(h(), t().takeWhile(p))
      case _ => empty
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }
//
//    def foldRight[B](z: => B, f: (A, => B) => B): B = this match {
//      case Cons(h, t) => f(h(), t().foldRight(z)(f))
//      case _ => z
//    }

    def forAll(p: A => Boolean): Boolean = this match {
      case Cons(h, t) => if (p(h())) t().forAll(p) else false
      case _ => true
    }

    def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = {
//      this.foldRight(empty, (x, y) => if (p(x)) cons(x, y) else empty)
//      val what: ((A, Stream[Int]) => Stream[Int]) => Stream[Int] = this.foldRight(empty[Int])

      this.foldRight(empty[A])((x, y) => if (p(x)) cons(x, y) else empty)
    }
  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  def cons[A](hd: => A, t1: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = t1
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) Empty
    else cons(as.head, apply(as.tail: _*))
  }

  def main(args: Array[String]) = {
    val str = cons({
      println(1)
      1
    }, empty)
    str.toList
    str.toList
    ()


    val x = cons({println("hi"); 5},  Stream(1,2,3,4))
//    println(Stream(1, 2, 3, 4, 5, 6).takeWhileViaFoldRight(x => x < 8).toList)
  }
}
