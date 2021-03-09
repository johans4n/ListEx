package co.s4n.listex

object ComprehensionList {
  /**
   * Obtiene el tamaño de una función utilizando for
   */
  def forLength[A](lst: List[A]): Int = (for {
    _ <- lst
  } yield ((a: Int) => a + 1)).foldLeft(0)((e, f) => f(e)
  )

  /**
   * Obtiene el último elemento de una lista
   */
  def forLast[A](lst: List[A]): A = (for {
    xi <- lst
  } yield ((a: A) => xi)).foldLeft(lst.head)((e, f) => f(e))

  /**
   * Retorna la cabeza de una lista
   */
  def forHead[A](lst: List[A]): A = (for {
    xi <- lst
  } yield ((a: A) => xi)).foldRight(lst.head)((f, e) => f(e))

  /**
   * retorna un elemento en una posición dada
   *
   */
  def forKElement[A](k: Int, lst: List[A]): A = (for {
    xi <- lst
  } yield ((t: (Int, Option[A])) => (t._2) match {
    case Some(x) => (t._1, Some(x))
    case None => if (t._1 == k) (t._1, Some(xi)) else (t._1 + 1, None)
  })).foldLeft((1, None: Option[A]))((e, f) => f(e))._2.get

}
