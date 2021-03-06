package co.s4n.listex


object ListEx extends App {

  def subs[A](lst: List[A]): List[List[A]] = lst match {
    case Nil => List(Nil)
    case head :: tail => subs(tail).map(head :: _) ::: subs(tail)
  }

  def barajar[A](a: A, lst: List[A]): List[List[A]] = lst match {
    case Nil => List(List(a))
    case head :: tail => (a :: (head :: tail)) :: ((barajar(a, tail).map(head :: _)))
  }

  def permute[A](lst: List[A]): List[List[A]] = lst match {
    case Nil => List(Nil)
    case head :: tail => (permute(tail)).flatMap(barajar(head, _))

  }

  def myLast[A](lst: List[A]): Option[A] = lst match {
    case Nil => None
    case head :: Nil => Some(head)
    case head :: tail => myLast(tail)
  }

  def myButLast[A](lst: List[A]): Option[A] = lst match {
    case Nil => None
    case head :: Nil => None
    case head :: tail :: Nil => Some(head)
    case head :: tail => myButLast(tail)
  }

  def penultimoYUltimo[A](lst: List[A]): List[A] = lst match {
    //case Nil => None
    //case head :: Nil =>
    case head :: tail :: Nil => List(head, tail)
    case head :: tail => penultimoYUltimo(tail)
  }

  def elementAt[A](lst: List[A], n: Int): A = (n, lst) match {
    case (1, head :: tail) => head
    case (n, head :: tail) => elementAt(tail, n - 1)
  }

  def mylenght[A](list: List[A]): Int = list match {
    case Nil => 0
    case head :: tail => 1 + mylenght(tail)
  }

  def mylenghtR[A](list: List[A]): Int = list.foldRight(0)((_, y) => 1 + y)


  def mylenghtL[A](list: List[A]): Int = list.foldLeft(0)((y, _) => y + 1)


  def isPalindrome[A](lst: List[A]): Boolean = lst == (lst).reverse



  sealed trait NList[+A]

  case class Elem[A](elem: A) extends NList[A]

  case class Const[A](lst: List[NList[A]]) extends NList[A]

  val y = Const(List(Elem(2), Const(List(Elem(1), Const(List(Elem(3), Elem(4))), Elem(5)))))

  def flatten[A](lst: NList[A]): List[A] = lst match {
    case Elem(value) => List(value)
    case Const(list) => list.flatMap(flatten)
  }

  def compress[A](lst: List[A]): List[A] = lst match {
    case ::(head, next) => head :: compress(next.dropWhile(head == _))
    case Nil => Nil
  }

/* def pack[A](lst: List[A]): List[A] = lst match {
    case ::(head, next) =>
    case Nil =>
  }
*/


}
