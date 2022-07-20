object Trees
{
  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
//3.25----------------------------------------------
  def size[A](t: Tree[A]): Int = t match
  {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }
  //----------------------------------------------

  //3.26----------------------------------------------
  def maximum(t: Tree[Int]): Int= t match
  {
    case Leaf(value) => value
    case Branch(l, r) => maximum(l).max(maximum(r))
  }
  //----------------------------------------------

  //3.27----------------------------------------------
  def depth[A](t: Tree[A]): Int= t match
  {
    case Leaf(value) => 0
    case Branch(l, r) => 1 + depth(l).max(depth(r))
  }
  //----------------------------------------------

  //3.28----------------------------------------------
  def map[A,B](t: Tree[A])(f: A => B): Tree[B]= t match
  {
    case Leaf(value) => Leaf(f(value))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }
  //----------------------------------------------

  //3.29----------------------------------------------
  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B= t match
  {
    case Leaf(value) => f(value)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeViaFold[A](t: Tree[A]): Int =
  {
    fold(t)(x => 1)((x, y)=>1+x+y)
  }

  def maximumViaFold(t: Tree[Int]): Int=
  {
    fold(t)(x=>x)((x,y)=> x.max(y))
  }

  def depthViaFold[A](t: Tree[A]): Int=
  {
    fold(t)(x=> 0)((x, y)=> 1 + x.max(y))
  }

  def mapViaFold[A,B](t: Tree[A])(f: A => B): Tree[B]=
  {
    fold(t)(x=>Leaf(f(x)):Tree[B])((x,y)=>Branch(x, y))
  }
  //----------------------------------------------

    def main(args: Array[String]): Unit =
    {
      println(mapViaFold(Branch(
        Branch(
          Leaf(12),
          Branch(
            Leaf(3),
            Leaf(4))),
        Leaf(8)))(x=> x+1))
    }
}
