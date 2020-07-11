package one.lab.tasks.week.three

object Collections extends App{

  // duplicateNTimes(3, List(1,2,3)) == List(1,1,1,2,2,2,3,3,3)
  // duplicateNTimes(3, List()) == List()
  def duplicateNTimes[A](n: Int, list: List[A]): List[A] = {
    def duplicate(list: List[A], dup:List[A]): List[A] = list match {
      case Nil       => List.empty[A]
      case x :: tail => if (tail.isEmpty) dup ::: nTimes(n, x) else duplicate(tail, dup ::: nTimes(n, x))
    }
    def nTimes(n: Int, x: A): List[A] = List.fill(n)(x)
    duplicate(list, List())
  }
//  println(duplicateNTimes(3, List(1,2,3)))
//  println(duplicateNTimes(3, List()))


  // splitAtK(4, List(1,2,3,4,5,6,7,8,9)) == (List(1,2,3,4), List(5,6,7,8,9))
  // splitAtK(0, List(1,2,3)) == (List(), List(1,2,3))
  def splitAtK[A](k: Int, list: List[A]): (List[A], List[A]) = {
    def split(n: Int, cList: List[A], pList: List[A]): (List[A], List[A]) = (n, cList) match {
      case (_, Nil) => (pList.reverse, Nil)
      case (0, list) => (pList.reverse, list)
      case (k, x :: tail) => split(k-1, tail, x :: pList)
    }
    split(k, list, Nil)
  }

//  println(splitAtK(4,List(1,2,3,4,5,6,7,8,9)))
//  println(splitAtK(0, List(1,2,3)))
//  println(splitAtK(0, Nil))

  // removeKthElement(5, List(1,2,3,4,5,6)) == (List(1,2,3,4,5), 6)
  // removeKthElement(2, List(1,2,3,4,5,6)) == (List(1,2,4,5,6), 2)
  // removeKthElement(-3, List(1,2,3,4,5,6)) == IndexOutOfBoundException
  // removeKthElement(1000, List(1,2,3,4,5,6)) == IndexOutOfBoundException
  def removeKthElement[A](k: Int, list: List[A]): (List[A], A) = {
    if (k < 0) throw new IndexOutOfBoundsException
    def nthElem(list: List[A], acc: (Int,Int), kList: List[A]): (List[A], A) = list match {
      case Nil       => throw new IndexOutOfBoundsException
      case x :: tail => if (acc._1 == acc._2) ((kList ::: tail), x) else nthElem(tail, (acc._1 +1, acc._2), (kList :+ x))
    }
    nthElem(list, (0,k), List())
  }

//  println(removeKthElement(5, List(1,2,3,4,5,6)))
//  println(removeKthElement(2, List(1,2,3,4,5,6)))
//  println(removeKthElement(-3, List(1,2,3,4,5,6)))
//  println(removeKthElement(1000, List(1,2,3,4,5,6)))
}
