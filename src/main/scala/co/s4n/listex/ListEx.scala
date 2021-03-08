package co.s4n.listex

import scala.util.Random


object ListEx extends App {

  /**
   * Genera los subconjuntos de una lista.
   *
   * @param lst
   * @tparam A
   * @return
   */
  def subs[A](lst: List[A]): List[List[A]] = lst match {
    case Nil => List(Nil)
    case head :: tail => subs(tail).map(head :: _) ::: subs(tail)
  }

  /**
   * retorna una lista con las combinaciones de un elemento frente a los demás elementos
   *
   * @param a
   * @param lst
   * @tparam A
   * @return
   */
  def barajar[A](a: A, lst: List[A]): List[List[A]] = lst match {
    case Nil => List(List(a))
    case head :: tail => (a :: (head :: tail)) :: ((barajar(a, tail).map(head :: _)))
  }

  /**
   * Genera todas las combinaciones de los elementos de una lista
   *
   * @param lst
   * @tparam A
   * @return
   */
  def permute[A](lst: List[A]): List[List[A]] = lst match {
    case Nil => List(Nil)
    case head :: tail => (permute(tail)).flatMap(barajar(head, _))

  }

  /**
   * Retorna el penultimo y el último objeto de una lista
   *
   * @param lst
   * @tparam A
   * @return
   */
  def penultimoYUltimo[A](lst: List[A]): List[A] = lst match {
    case Nil => Nil
    //case head :: Nil =>
    case head :: tail :: Nil => List(head, tail)
    case head :: tail => penultimoYUltimo(tail)
  }

  /**
   * Ejercicio 1 myLast Retorna el último objeto de una lista
   *
   * @param lst
   * @tparam A
   * @return
   */
  def myLast[A](lst: List[A]): Option[A] = lst match {
    case Nil => None
    case head :: Nil => Some(head)
    case head :: tail => myLast(tail)
  }

  /**
   * Ejercicio 2 myButLast Retorna el penúltimo objeto de una lista
   *
   * @param lst
   * @tparam A
   * @return
   */
  def myButLast[A](lst: List[A]): Option[A] = lst match {
    case Nil => None
    case head :: Nil => None
    case head :: tail :: Nil => Some(head)
    case head :: tail => myButLast(tail)
  }


  /**
   * Ejercicio 3 meno Retorna un elemento pedido en una posición dada.
   *
   * @param lst
   * @param n
   * @tparam A
   * @return
   */
  def elementAt[A](lst: List[A], n: Int): A = (n, lst) match {
    case (0, head :: tail) => head
    case (1, head :: tail) => head
    case (n, head :: tail) => elementAt(tail, n - 1)
  }

  /**
   * Ejercicio 4 Retorna el tamaño de una lista
   *
   * @param list
   * @tparam A
   * @return
   */
  def mylenght[A](list: List[A]): Int = list match {
    case Nil => 0
    case head :: tail => 1 + mylenght(tail)
  }

  /**
   * Retorna el tamaño de una lista usando foldRight
   *
   * @param list
   * @tparam A
   * @return
   */
  def mylenghtR[A](list: List[A]): Int = list.foldRight(0)((_, y) => 1 + y)

  /**
   * Retorna el tamaño de una usando foldLeft
   *
   * @param list
   * @tparam A
   * @return
   */
  def mylenghtL[A](list: List[A]): Int = list.foldLeft(0)((y, _) => y + 1)

  /**
   * Ejercicio 5 myReverse
   *
   * @param lst
   * @tparam A
   * @return
   */
  def myReverse[A](lst: List[A]): List[A] = lst match {
    case head :: tail => myReverse(tail) ::: List(head)
    case Nil => Nil
  }

  /**
   * Ejercicio 6 Retorna true si los elementos de una lista son palíndromos
   *
   * @param list
   * @tparam A
   * @return
   */
  def isPalindrome[A](lst: List[A]): Boolean = lst == (lst).reverse

  /*-------------------------------------------Trait nested list--------------------------------------------------------*/
  sealed trait NList[+A]

  case class Elem[A](elem: A) extends NList[A]

  case class Const[A](lst: List[NList[A]]) extends NList[A]

  /**
   * Ejercicio 7 aplana un arbol de listas en una lista simple.
   *
   * @param lst
   * @tparam A
   * @return
   */
  def flatten[A](lst: NList[A]): List[A] = lst match {
    case Elem(value) => List(value)
    case Const(list) => list.flatMap(flatten)
  }

  /**
   * Ejercicio 8 Elimina elementos repetidos de una lista
   *
   * @param lst
   * @tparam A
   * @return
   */
  def compress[A](lst: List[A]): List[A] = lst match {
    case ::(head, next) => head :: compress(next.dropWhile(head == _))
    case Nil => Nil
  }

  /**
   * Ejercicio 9 separa elementos repetidos por listas distintas
   *
   * @param lst
   * @tparam A
   * @return
   */
  def pack[A](lst: List[A]): List[List[A]] = lst match {
    case Nil => Nil
    case x :: tail => val (head, tail) = lst.span(x == _); head :: pack(tail)
  }

  /**
   * Ejercicio 10 genera una lista con tuplas que se componen de la cantidad de objetos de una lista y el objeto
   *
   * @param lst
   * @tparam A
   * @return
   */
  def encode[A](lst: List[A]): List[(Int, A)] = pack(lst) match {
    case Nil => Nil
    case lst => lst.map(lista => (lista.length, lista.head))
  }

  /**
   * Ejercicio 11 genera una lista con tuplas que se componen de la cantidad de objetos de una lista de elementos duplicados
   * y retornando un único objeto cuando no tenga elementos duplicados
   *
   * @param lst
   * @tparam A
   * @return
   */
  def encodeModified[A](lst: List[A]): List[_] = pack(lst) match {
    case Nil => Nil
    case lst => lst.map(lista => {
      if (lista.length > 1) (lista.length, lista.head)
      else lista.head
    })
  }

  /**
   * Ejercicio 12 retorna una lista resultado del proceso inverso a encode
   *
   * @param lst
   * @tparam A
   * @return
   */
  /*  def decodeModified[A](lst: List[(A,A)]): Any = lst match {
      case Nil =>
      case (head:: tail) => println(head._1); println(head._2); decodeModified(tail)
    }*/

  def decodeModified[A](lst: List[A]): Any = lst match {
    case Nil => Nil
    case List() => print(" ")
    case List((n, m)) =>  println(m); decodeModified(lst.tail)
    case lst => println(lst.head); decodeModified(lst.tail)
  }


  /**
   * 22
   *
   * @param from
   * @param to
   * @tparam A
   * @return
   */
  def range[A](from: Int, to: Int): List[Int] = {
    def aux[A](from: Int, to: Int, lstAcc: List[Int]): List[Int] = (from compare to) match {
      case 0 => lstAcc ::: List(to)
      case -1 => aux(from + 1, to, lstAcc ::: List(from))

    }

    aux(from, to, Nil)
  }

  /**
   * 23
   *
   * @param lst
   * @param n
   * @tparam A
   * @return
   */
  def myrndSelect[A](lst: List[A], n: Int): List[A] = {
    def aux(lst: List[A], lstAcc: List[A], n: Int): List[A] = n match {
      case 0 => lstAcc
      case n => aux(lst, (elementAt(lst, Random.nextInt(lst.length)) :: lstAcc), n - 1)
    }

    aux(lst, Nil, n)
  }

  /**
   * 24
   *
   * @param n
   * @param max
   * @tparam A
   * @return
   */
  def mydiffSelect[A](n: Int, max: Int): List[Int] = myrndSelect(range(1, max), n)

  /**
   * 25
   *
   * @param lst
   * @tparam A
   * @return
   */
  def myRndmPermute[A](lst: List[A]): List[A] = myrndSelect(permute(lst), 1).flatten

  def myRndmPermute2[A](lst: List[A]): List[A] = {
    val permutada = permute(lst)
    elementAt(permutada, Random.nextInt(permutada.length))
  }

  /**
   * 26
   *
   * @param lst
   * @param k
   * @tparam A
   * @return
   */
  def mycombinations[A](lst: List[A], k: Int): List[List[A]] = lst match {
    case head :: tail if k == 1 => lst.map(List(_))
    case head :: tail => (mycombinations(tail, k - 1).flatMap(barajar(head, _)))
    case Nil => List(Nil)
  }
}
