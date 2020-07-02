//package one.lab.demos.week.one
//
//import scala.annotation.tailrec
//import math._
//
//object Boot extends App {
//  // syntax
//  // https://ru.wikipedia.org/wiki/%D0%92%D1%8B%D0%B2%D0%BE%D0%B4_%D1%82%D0%B8%D0%BF%D0%BE%D0%B2
//  val camelCase         = 3
//  val SomeConstantValue = 3.14
//
//  def camelCaseMethod: String = "output"
//
//  class CamelCaseClass()
//  object CamelCaseObject {}
//  println("-" * 100)
//  // data Types
//
//  val float: Float     = 3.2f
//  val long: Long       = 100000000L
//  val double: Double   = 23.123123
//  val boolean: Boolean = true & false
//
//  val unit: Unit = {
//    34324
//    342
//    ()
//  }
//
//  val and: Boolean = true & false
//  val xor: Boolean = true ^ false
//  val or: Boolean  = true | false
//  val shiftLeft    = 1 << 2
//  val shiftRight   = 16 >> 1
//
//  println(shiftLeft)  // 4
//  println(shiftRight) // 8
//
//  def getSomeString: String = "dsfdsf"
//
//  var someState = 1
//
//  def updateState(): Unit = someState + 1
//
//  if (boolean) println("-" * 100)
//  // var/val
//
//  //  var - переменные
//  //  val - значение
//  val someValue = 3
//  //  someValue = 5 impossible
//  var someVariable = 5
//  someVariable += 1
//
//  var l = List(1, 2, 3, 4)
//  println(l.map(x => x * 2)) // List(2, 4, 6, 8) function: (Int => Int)
//  println(l)                 // List(1, 2, 3, 4)
//  l = List(2, 3, 9, 10)
//  println(l)
//
//  println("-" * 100)
//
//  // methods
//
//  def someOutsideMethod(param1: String, param2: Int): String = ???
//
//  println("-" * 100)
//  // functions
//  val x: Int => String = x => x.toString
//  println("-" * 100)
//
//  // currying
//  def sum(initial: Int)(a: Int, b: Int): Int = initial + a + b
//  val sumWithZero: (Int, Int) => Int         = (a, b) => sum(3)(a, b)
//
//  println(sumWithZero(2, 3))
//
//  println("-" * 100)
//  // lazy val
//
//  lazy val someComputation: Int = {
//    Thread.sleep(2000)
//    42
//  }
//
//  println("Hey i'm before lazy val")
//  println(someComputation)
//  println("I was computed")
//  println(someComputation)
//
//  println("-" * 100)
//  // if/else
//
//  val inlineIf = if (someComputation == 42) "OK" else if (true) "NOT OK" else if (true) "" else ""
//
//  println("-" * 100)
//  // while loops
//  var iterator = 0
//  while (iterator < 10) {
//    println(iterator)
//    iterator += 1
//  }
//
//  println("-" * 100)
//  // recursion/tail recursion
//
//  def factorial(value: Int): Int =
//    if (value <= 1) 1
//    else value * factorial(value - 1)
//
//  println(factorial(5)) // 120
//  var someStateOfClass = 1
//
//  // under the hood will be translated to loop
//  def factorialTailRec(value: Int): Int = {
//    @tailrec
//    def innerTailRec(accumulator: Int, it: Int): Int =
//      if (it <= 1) accumulator
//      else innerTailRec(accumulator * it, it - 1)
//
//    innerTailRec(1, value)
//  }
//
//  println(factorialTailRec(10))
//
//  println("-" * 100)
//
//  // classes
//  class User(val name: String, val email: String) {
//    def authorize(password: String): Boolean              = ???
//    def authorize(token: String, `type`: String): Boolean = ???
//    def authorize(code: Int): Long                        = ???
//
//    override def toString: String = s"$name,$email"
//    override def hashCode(): Int  = (name + " " + email).hashCode
//  }
//
//  val user = new User("", "")
//  user.name
//
//  println("-" * 100)
//
//  // objects
//  // singleton OOP
//  object Utils {
//    class Json {}
//    def parseJson(string: String): Json = ???
//  }
//
//  println("-" * 100)
//
//  // traits
//  // OOP - Inheritance
//  trait Laptop {
//    val brand: String
//    val model: String
//  }
//
//  trait SamsungLaptop extends Laptop {
//    override val brand: String = "Samsung"
//  }
//
//  trait LenovaLaptop extends Laptop
//
//  class SamsungTabletLaptop extends SamsungLaptop {
//    override val model: String = "Tablet/1.3"
//  }
//
//  class LenovaYogaLaptop extends LenovaLaptop {
//    override val brand: String = "Lenova"
//    override val model: String = "Yoga/180"
//  }
//
//  def prettyPrint(laptop: Laptop): String =
//    s"${laptop.brand} - ${laptop.model}"
//
//  println(prettyPrint(new LenovaYogaLaptop()))
//}
