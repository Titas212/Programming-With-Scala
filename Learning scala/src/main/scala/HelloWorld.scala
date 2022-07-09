object HelloWorld
{

  def multiply(x:Int, y:Int) : Int =
  {
    x*y
  }

  def formatString(x: Int, y: Int) =
    {
      val message = "%d multiplied by %d is equal to %d"
      message.format(x, y, multiply(x, y))
    }

  def calcFactorial(number : Int) : Int =
    {
      def go(n: Int, acc : Int) : Int =
        if(n<=0) acc
        else go(n-1, n*acc)

      go(number, 1)
    }

  def addAll(number: Int) : Int =
    {
      def go(n: Int, sum : Int): Int =
        if(n==0) sum
        else go(n-1, n+sum)

      go(number,0)
    }

  def fibonacci (n: Int) : Int =
    {
      def go (place : Int, prev: Int, sum: Int): Int =
      {
        if(place == n) prev
        else go(place+1, sum, prev+sum)
      }

      go(0,0,1)

    }

  def main(args: Array[String]): Unit =
    {
      println(formatString(5, 4))
    }
}
