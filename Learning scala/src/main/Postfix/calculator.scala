import java.util
import scala.util.{Failure, Success, Try}

object calculator {

  sealed trait Token
  case object + extends Token
  case object - extends Token
  case object * extends Token
  case object / extends Token
  case class number(num: Double) extends Token

  def parseString2(str: String): Either[String, List[Token]] =
  {
    val x = str.split(" ").toList
    sequence(x.map(x => assignToken(x)))
  }

  def assignToken(str: String): Either[String, Token] = str match
    {
    case "+" => Right(+)
    case "-" => Right(-)
    case "*" => Right(*)
    case "/" => Right(/)
    case _ => Try(str.toDouble) match
    {
      case Failure(exception) => Left("Invalid token")
      case Success(value) => Right(number(value))
    }
  }


  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
  {
    traverse(es)(x => x)
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match
  {
    case Nil => Right(Nil)
    case h::t => f(h).flatMap(x => traverse(t)(f).map(x:: _))
  }

  def operate(num1: Double, num2: Double, operator: Token): Double =
    {
      operator match
        {
        case + => num2 + num1
        case - => num2 - num1
        case * => num2 * num1
        case / => num2 / num1
      }
    }

  def execute(str: String): Either[String, Double] =
    {
      val li = parseString2(str)
      var i = 0
      val stack = new util.Stack[Double]()
      li match
      {
        case Right(x) =>
          while(i<x.length)
            {
              x(i) match
              {
                case number(x) =>
                stack.push(x)
                  i = i+1
                case _ =>
                stack.push(operate(stack.pop(), stack.pop(), x(i)))
                  i = i+1
              }
            }
        case Left(x) => return Left(x)
      }
      Right(stack.pop())
    }

  def main(args: Array[String]) =
  {
    println(execute("3 4 7 * 2 / +"))
  }
}
