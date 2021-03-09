package co.s4n.listex

import scala.annotation.tailrec
import scala.util.Random


object ListEx extends App {

  /**
   * Taller ejercicio 1
   * Genera los subconjuntos de una lista.
   */
  def subs[A](lst: List[A]): List[List[A]] = lst match {
    case Nil => List(Nil)
    case head :: tail => subs(tail).map(head :: _) ::: subs(tail)
  }

  /**
   * Taller ejercicio 2
   * Genera todas las combinaciones de los elementos de una lista
   */
  def permute[A](lst: List[A]): List[List[A]] = lst match {
    case Nil => List(Nil)
    case head :: tail => (permute(tail)).flatMap(barajar(head, _))

  }

  /**
   * retorna una lista con las combinaciones de un elemento frente a los demás elementos
   */
  def barajar[A](a: A, lst: List[A]): List[List[A]] = lst match {
    case Nil => List(List(a))
    case head :: tail => (a :: (head :: tail)) :: ((barajar(a, tail).map(head :: _)))
  }

  /**
   * Retorna el penultimo y el último objeto de una lista
   */
  @tailrec
  def penultimoYUltimo[A](lst: List[A]): List[A] = lst match {
    case Nil => Nil
    //case head :: Nil =>
    case head :: tail :: Nil => List(head, tail)
    case head :: tail => penultimoYUltimo(tail)
  }

  /*------------------------------------------EJERCICIOS 1-99  HASKELL ---------------------------------------*/

  /**
   * Ejercicio 1 myLast Retorna el último objeto de una lista
   */
  @tailrec
  def myLast[A](lst: List[A]): Option[A] = lst match {
    case Nil => None
    case head :: Nil => Some(head)
    case head :: tail => myLast(tail)
  }

  /**
   * Ejercicio 2 myButLast Retorna el penúltimo objeto de una lista
   */
  @tailrec
  def myButLast[A](lst: List[A]): Option[A] = lst match {
    case Nil => None
    case head :: Nil => None
    case head :: tail :: Nil => Some(head)
    case head :: tail => myButLast(tail)
  }

  /**
   * Ejercicio 3 meno Retorna un elemento pedido en una posición dada.
   */
  @tailrec
  def elementAt[A](lst: List[A], n: Int): A = (n, lst) match {
    case (0, head :: tail) => head
    case (1, head :: tail) => head
    case (n, head :: tail) => elementAt(tail, n - 1)
  }

  /**
   * Ejercicio 4 Retorna el tamaño de una lista
   */
  def mylenght[A](list: List[A]): Int = list match {
    case Nil => 0
    case head :: tail => 1 + mylenght(tail)
  }

  /**
   * Ejercicio 4  Retorna el tamaño de una lista usando foldRight
   */
  def mylenghtR[A](list: List[A]): Int = list.foldRight(0)((_, y) => 1 + y)

  /**
   * Ejercicio 4  Retorna el tamaño de una usando foldLeft
   */
  def mylenghtL[A](list: List[A]): Int = list.foldLeft(0)((y, _) => y + 1)

  /**
   * Ejercicio 5 myReverse
   */
  def myReverse[A](lst: List[A]): List[A] = lst match {
    case head :: tail => myReverse(tail) ::: List(head)
    case Nil => Nil
  }

  /**
   * Ejercicio 6 Retorna true si los elementos de una lista son palíndromos
   */
  def isPalindrome[A](lst: List[A]): Boolean = lst == (lst).reverse


  /*-------------------------------------------Trait nested list------------------------------------------------------*/
  sealed trait NList[+A]

  case class Elem[A](elem: A) extends NList[A]

  case class Const[A](lst: List[NList[A]]) extends NList[A]

  /**
   * Ejercicio 7 aplana un arbol de listas en una lista simple.
   */
  def flatten[A](lst: NList[A]): List[A] = lst match {
    case Elem(value) => List(value)
    case Const(list) => list.flatMap(flatten)
  }

  /**
   * Ejercicio 8 Elimina elementos repetidos de una lista
   */
  def compress[A](lst: List[A]): List[A] = lst match {
    case ::(head, next) => head :: compress(next.dropWhile(head == _))
    case Nil => Nil
  }

  /**
   * Ejercicio 9 separa elementos repetidos por listas distintas
   */
  def pack[A](lst: List[A]): List[List[A]] = lst match {
    case Nil => Nil
    case x :: tail => val (head, tail) = lst.span(x == _); head :: pack(tail)
  }

  /**
   * Ejercicio 10 genera una lista con tuplas que se componen de la cantidad de objetos de una lista y el objeto
   */
  def encode[A](lst: List[A]): List[(Int, A)] = pack(lst) match {
    case Nil => Nil
    case lst => lst.map(lista => (lista.length, lista.head))
  }

  /**
   * Ejercicio 11 genera una lista con tuplas que se componen de la cantidad de objetos de una lista de elementos duplicados
   * y retornando un único objeto cuando no tenga elementos duplicados
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
   */
  def decodeModified[A](lst: List[A]): List[_] = {
    /**
     * Retorna una lista de elementos repetidos dependiendo del numero de veces que le enviemos
     */
    def decodeAux[A](n: Int, a: A): List[A] = n match {
      case 1 => List(a)
      case n => a :: decodeAux(n - 1, a)
    }

    lst match {
      case Nil => Nil
      case list => list.head match {
        case (x: Int, y) => decodeAux(x, y) ::: decodeModified(list.tail)
        case x => List(x) ::: decodeModified(list.tail)
      }
    }
  }

  /**
   * Ejercicio 13 realiza la codificacion de las listas ignorando las tuplas de length 1
   */
  def encodeDirect[A](lst: List[A]): List[_] = pack(lst) match {
    case Nil => Nil
    case lst => lst.map(lista => {
      if (lista.length > 1) (lista.length, lista.head)
      else lista.head
    })
  }

  /**
   * Ejercicio 14 duplica los elementos de una lista
   */
  def dupli[A](lst: List[A]): List[A] = lst match {
    case Nil => Nil
    case head :: tail => head :: head :: dupli(tail)
  }

  /**
   * Ejercicio 15 La funcion repli replica n veces los elementos de una lista.
   */
  def repli[A](lst: List[A], n: Int): List[A] = {
    @tailrec
    def aux[A](lst: List[A], naux: Int, listAcc: List[A]): List[A] = lst match {
      case x :: xs => if (n != naux) aux(lst, naux + 1, listAcc ::: List(x)) else aux(xs, 0, listAcc)
      case Nil => listAcc
    }

    aux(lst, 0, Nil)
  }

  /**
   * Ejercicio 16 elimina cada n elementos de la lista
   */
  def drop[A](lst: List[A], n: Int): List[A] = {
    @tailrec
    def aux[A](lst: List[A], n2: Int, acc: List[A]): List[A] = lst match {
      case x :: xs => if (n == n2) aux(xs, 1, acc) else aux(xs, n2 + 1, acc ::: List(x))
      case Nil => acc
    }

    aux(lst, 1, Nil)
  }

  /**
   * Ejercicio 17 divide la lista en dos partes dependiendo de la posición que le enviemos.
   */
  def split[A](n: Int, lst: List[A]): (List[A], List[A]) = {
    @tailrec
    def splitAux[A](c: Int, lst2: List[A], acum: List[A]): (List[A], List[A]) = (c, lst2) match {
      case (n, Nil) => (acum, lst2)
      case (0, l) => (acum, lst2)
      case (c, head :: tail) => splitAux(c - 1, tail, acum :+ head)
    }

    splitAux(n, lst, Nil)
  }

  /**
   * Ejercicio 18 dados dos indices extrae la lista entre esos dos indices.
   */
  def slice[A](lst: List[A], i1: Int, i2: Int): List[A] = {
    def aux[A](lst: List[A], lstAcc: List[A], contador: Int): List[A] = lst match {
      case head :: tail => {
        if (i1 > contador || contador > i2) aux(tail, lstAcc, contador + 1)
        else aux(tail, lstAcc ::: List(head), contador + 1)
      }
      case Nil => lstAcc
    }

    aux(lst, Nil, 1)
  }

  /**
   * Ejercicio 20. Remueve un elemento de una lista dependiendo del indice que le demos.
   */
  def removeAt[A](lst: List[A], n: Int): (List[A], List[A]) = {
    def aux(lst: List[A], lst1: List[A], lst2: List[A], n2: Int): (List[A], List[A]) = lst match {
      case x :: xs => if (n == n2) aux(xs, x :: lst1, lst2, n2 + 1)
      else aux(xs, lst1, lst2 ::: List(x), n2 + 1)
      case Nil => (lst1, lst2)
    }

    aux(lst, Nil, Nil, 1)
  }

  /**
   * Ejercicio 21 Inserta un elemento en una posición dada
   */
  def insertAt[A](elem: A, lst: List[A], position: Int): List[A] = {
    def aux[A](elem: A, lst: List[A], lstAcc: List[A], n2: Int): List[A] = lst match {
      case x :: xs => if (position == n2) aux(elem, xs, lstAcc ::: List(elem) ::: List(x), n2 + 1)
      else aux(elem, xs, lstAcc ::: List(x), n2 + 1)
      case Nil => lstAcc
    }

    aux(elem, lst, Nil, 1)
  }

  /**
   * Ejercicio 22 Genera una lista con un rango de números
   */
  def range[A](from: Int, to: Int): List[Int] = {
    @tailrec
    def aux[A](from: Int, to: Int, lstAcc: List[Int]): List[Int] = (from compare to) match {
      case 0 => lstAcc ::: List(to)
      case -1 => aux(from + 1, to, lstAcc ::: List(from))

    }

    aux(from, to, Nil)
  }

  /**
   * Ejercicio 23 extrae elementos aleatoriamente  de una lista a partir de una cantidad dada
   */
  def myrndSelect[A](lst: List[A], n: Int): List[A] = {
    @tailrec
    def aux(lst: List[A], lstAcc: List[A], n: Int): List[A] = n match {
      case 0 => lstAcc
      case n => aux(lst, (elementAt(lst, Random.nextInt(lst.length)) :: lstAcc), n - 1)
    }

    aux(lst, Nil, n)
  }

  /**
   * Ejercicio 24 Crea una lista de números aleatorios en un rango de números
   */
  def mydiffSelect[A](n: Int, max: Int): List[Int] = myrndSelect(range(1, max), n)

  /**
   * Ejercicio 25 Genera una permutación aleatoria de una lista
   */
  def myRndmPermute[A](lst: List[A]): List[A] = myrndSelect(permute(lst), 1).flatten

  /**
   * forma 2 (mas optimizada)
   */
  def myRndmPermute2[A](lst: List[A]): List[A] = {
    val permutada = permute(lst)
    elementAt(permutada, Random.nextInt(permutada.length))
  }

  /**
   * Ejercicio 26 genera combinaciones de una lista a partir de un tamaño dado
   */
  def mycombinations[A](lst: List[A], k: Int): List[List[A]] = lst match {
    case head :: tail if k == 1 => lst.map(List(_))
    case head :: tail => (mycombinations(tail, k - 1).flatMap(barajar(head, _)))
    case Nil => List(Nil)
  }


}
