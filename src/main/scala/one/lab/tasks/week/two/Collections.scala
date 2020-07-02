package one.lab.tasks.week.two

object Collections extends App{
  // getLast(List(1 ,2, 3, 4)) -> 4
  // getLast(List())           -> java.util.NoSuchElementException
  def getLast[A](list: List[A]): A = list match {
    case Nil => throw new NoSuchElementException()
    case x :: tail => if (tail.isEmpty) x else getLast(tail)
  }

//  println(getLast(List(1 ,2, 3, 4)))
//  println(getLast(List()))

  // getLastOption(List(1 ,2, 3, 4)) -> Some(4)
  // getLastOption(List())           -> None
  def getLastOption[A](list: List[A]): Option[A] = list match {
    case Nil => None
    case x :: tail => if (tail.isEmpty) Some(x) else getLastOption(tail)
  }

//  println(getLastOption(List(1,2,3,4)))
//  println(getLastOption(List()))

  // getPreLast(List(1 ,2, 3, 4)) -> 3
  // getPreLast(List(1))          -> java.util.NoSuchElementException
  // getPreLast(List())           -> java.util.NoSuchElementException
  def getPreLast[A](list: List[A]): A = list match {
    case Nil => throw new NoSuchElementException()
    case x :: tail => if (tail.length == 1) x else getPreLast(tail)
  }

//  println(getPreLast(List(1,2,3,4)))
//  println(getPreLast(List(1)))
//  println(getPreLast(List()))

  // getPreLastOption(List(1 ,2, 3, 4)) -> Some(3)
  // getPreLastOption(List(1))          -> None
  // getPreLastOption(List())           -> None
  def getPreLastOption[A](list: List[A]): Option[A] = list match {
    case Nil => None
    case x :: tail => if (tail.length == 1) Some(x) else getPreLastOption(tail)
  }

//  println(getPreLastOption(List(1,2,3,4)))
//  println(getPreLastOption(List(1)))
//  println(getPreLastOption(List()))

  // getNthElement(3, List(1 ,2, 3, 4)) -> 3
  // getNthElement(3, List(1))          -> java.lang.IndexOutOfBoundsException
  def getNthElement[A](n: Int, list: List[Int]): Int = {

    def nthElem(list: List[Int], acc: (Int, Int)): Int = list match {
      case Nil => throw new NoSuchElementException
      case x :: tail => if (acc._1 == acc._2) x else nthElem(tail, (acc._1 + 1, acc._2))
    }
    nthElem(list, (0,n))

  }

  println(getNthElement(3, List(1 ,2, 3, 4)))

  // getNthElementOption(3, List(1 ,2, 3, 4)) -> Some(3)
  // getNthElementOption(3, List(1))          -> None
  def getNthElementOption[A](n: Int, list: List[A]): Option[A] = {
    def nthElem(list: List[A], acc: (Int,Int)): Option[A] = list match {
      case Nil => None
      case x :: tail => if(acc._1 == acc._2) Some(x) else nthElem(tail, (acc._1 +1, acc._2))
    }
    nthElem(list, (0,n))
  }

  println(getNthElementOption(3, List(1 ,2, 3, 4)))

  // getLength(List(1,2,3)) -> 3
  // getLength(List())      -> 0
  def getLength[A](list: List[A]): Int = {
    def length(list: List[A], n: Int): Int = list match {
      case Nil => 0
      case x :: tail => if(tail.isEmpty) n else length(tail, n+1)
    }
    length(list, 1)
  }

  println(getLength(List()))

  // getReversedList(List(1,2,3)) -> List(3,2,1)
  def getReversedList[A](list: List[A]): List[A] = {
    def reverse(list: List[A], rev: List[A]): List[A] = list match {
      case Nil => throw new NoSuchElementException()
      case x :: tail => if(tail.isEmpty) x +: rev else reverse(tail, x +: rev)
    }
    reverse(list, List())
  }

  println(getReversedList(List(1,2,3)))

  // duplicateEveryElement(List(1,2,3)) -> List(1,1,2,2,3,3)
  def duplicateEveryElement[A](list: List[A]): List[A] = {
    def duplicate(list: List[A], dup: List[A]): List[A] = list match {
      case Nil => throw new NoSuchElementException()
      case x :: tail => if(tail.isEmpty) dup ::: times(x) else duplicate(tail, dup ::: times(x) )
    }
    def times(n: A): List[A] =  n :: n :: Nil//List.fill(2)(n)
    duplicate(list, List())
  }

  println(duplicateEveryElement(List(1,2,3)))
}
