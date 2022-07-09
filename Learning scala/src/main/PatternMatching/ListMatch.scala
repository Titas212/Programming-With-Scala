import sun.reflect.generics.tree.IntSignature

import scala.collection.View.Empty
import scala.collection.immutable.{List, _}

trait ListMatch[+A]
case object  Nil extends ListMatch[Nothing]
case class ::[+A](head: A, tail: ListMatch[A]) extends ListMatch[A]

object ListMatch
{

  def matchInts(ints: ListMatch[Int]): Int = ints match
  {
    case ::(x, ::(2, ::(4, _))) => x
    case Nil => 42
    case ::(x, ::(y, ::(3, ::(4, _)))) => x + y
    case _ => 101
  }

  def matchStrings(strings: ListMatch[String]) : String = strings match {
    case ::("labas", _) => "Hello"
    case Nil => "Empty"
    case ::("Laba", ::("vakara", _)) => "Good evening"
    case _ => "Anything"
  }
  def polymorphicExample[A](as: Array[A], a: A): Boolean =
    {
      def go(iterate: Int): Boolean =
      {
        if(a.equals(as(iterate))) true
        else if(iterate == as.length-1) false
        else go(iterate + 1)
      }
      go(0)
    }

  def apply[A](as: A*): ListMatch[A] =
    {
      if(as.isEmpty) Nil
      else ::(as.head, apply(as.tail: _*))
    }

  def main(args: Array[String])=
  {

  }
}
