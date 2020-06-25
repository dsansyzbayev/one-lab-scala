package one.lab.tasks.week.one

object Recursion extends App{
  def printNTimes(n: Int, value: String): Unit = {
    if(n==0) return //Not sure if it is ok to use it
    print(s"$value ")
    printNTimes(n-1, value)
  }
  //simple solution without recurison ==>
  // def printNTime(n: Int, value: String): Unit = println(s"$value\n" * n)

  printNTimes(4, "test")

  def gcd(a: Long, b: Long): Long = {
      if (b == 0) a
      else gcd(b, a % b)
  }

  def nthFibonacciNumber(n: Int): Int = {
    if (n <= 0) {
      0
    } else if (n <= 1) {
      1
    } else {
      nthFibonacciNumber(n-1) + nthFibonacciNumber(n-2)
    }
  }

  def tailRecursiveFibonacciNumber(n: Int): Int = {
    @scala.annotation.tailrec
    def tailFib(n: Int, prev: Int, cur: Int): Int = {
      if (n<=0) cur
      else tailFib(n-1, prev = prev + cur, cur = prev)
    }
    tailFib(n, 1, 0)
  }
}
