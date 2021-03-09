package co.s4n.listex

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ListExSpec extends AnyFlatSpec with Matchers {

  "1. subs(List('A','B','C')" should "List(List(), List('A'), List('B'), List('C'), List('A', 'B'), List('A','C'), " +
    "List('B', 'C'), List('A', 'B', 'C'))" in {
    val list = List("A", "B", "C")
    val result = List(List(), List("A"), List("B"), List("C"), List("A", "B"), List("A", "C"), List("B", "C"), List("A", "B", "C"))
    ListEx.subs(list).sortBy(_.length) shouldEqual result
  }

  "2. subs(List('A','B')" should "List(List(), List('A'), List('B'), List('A', 'B'))" in {
    val list = List("A", "B")
    val result = List(List(), List("A"), List("B"), List("A", "B"))
    ListEx.subs(list).sortBy(_.length) shouldEqual result
  }

  "3. subs(List(1,2)" should "List(List(), List(1), List(2), List(1, 2))" in {
    val list = List(1, 2)
    val result = List(List(), List(1), List(2), List(1, 2))
    ListEx.subs(list).sortBy(_.length) shouldEqual result
  }

  "4. permute(List(1,2))" should "List(List(1, 2), List(2, 1))" in {
    val list = List(1, 2)
    val result = List(List(1, 2), List(2, 1))
    ListEx.permute(list) shouldEqual result
  }

  "5. permute(List(1,2,3))" should "List(List(1, 2, 3), List(2, 1, 3), List(2, 3, 1), List(1, 3, 2), List(3, 1, 2), List(3, 2, 1))" in {
    val list = List(1, 2, 3)
    val result = List(List(1, 2, 3), List(2, 1, 3), List(2, 3, 1), List(1, 3, 2), List(3, 1, 2), List(3, 2, 1))
    ListEx.permute(list) shouldEqual result
  }

  "6. permute(List('A,'B','C'))" should "List(List(A, B, ), List(B, A, C), List(B, C, A), " +
    "List(A, C, B), List(C, A, B), List(C, B, A))" in {
    val list = List("A", "B", "C")
    val result = List(List("A", "B", "C"), List("B", "A", "C"), List("B", "C", "A"), List("A", "C", "B"), List("C", "A", "B"), List("C", "B", "A"))
    ListEx.permute(list) shouldEqual result
  }

  "7. myLast(List(1,2,3))" should "3" in {
    val list = List(1, 2, 3)
    val result = 3
    ListEx.myLast(list).get shouldEqual result
  }

  "8. myLast(List())" should "none" in {
    val list = List()
    val result = None
    ListEx.myLast(list) shouldEqual result
  }

  "9. myLast(Nil)" should "none" in {
    val list = Nil
    val result = None
    ListEx.myLast(list) shouldEqual result
  }

  "10. myButLast(List(1,2,3))" should "2" in {
    val list = List(1, 2, 3)
    val result = 2
    ListEx.myButLast(list).get shouldEqual result
  }

  "11. myButLast(List())" should "none" in {
    val list = List()
    val result = None
    ListEx.myButLast(list) shouldEqual result
  }

  "12. myButLast(Nil)" should "none" in {
    val list = Nil
    val result = None
    ListEx.myButLast(list) shouldEqual result
  }

  "13. myButLast(List('A'))" should "none" in {
    val list = List('A')
    val result = None
    ListEx.myButLast(list) shouldEqual result
  }

  "14. myButLast(List('A', 'B'))" should "none" in {
    val list = List("A", "B")
    val result = "A"
    ListEx.myButLast(list).get shouldEqual result
  }

  "15. penultimoYUltimo(List(1,2,3))" should "List(2,3)" in {
    val list = List(1, 2, 3)
    val result = List(2, 3)
    ListEx.penultimoYUltimo(list) shouldEqual result
  }

  "16. penultimoYUltimo(List())" should "Nil" in {
    val list = List()
    val result = Nil
    ListEx.penultimoYUltimo(list) shouldEqual result
  }

  "17. penultimoYUltimo(Nil)" should "none" in {
    val list = Nil
    val result = Nil
    ListEx.penultimoYUltimo(list) shouldEqual result
  }

  "18. penultimoYUltimo(List('A'))" should "none" in {
    val list = List('A')
    val result = Nil
    ListEx.penultimoYUltimo(list) shouldEqual result
  }

  "19. penultimoYUltimo(List('A', 'B'))" should "none" in {
    val list = List("A", "B")
    val result = List("A", "B")
    ListEx.penultimoYUltimo(list) shouldEqual result
  }

  "20. elementAt(List(1,2,3,4,5,6),2)" should "2" in {
    val list = List(1, 2, 3, 4, 5, 6)
    val result = 2
    ListEx.elementAt(list, 2) shouldEqual result
  }

  "21. elementAt(List(1,2,3,4,5,6),0)" should "1" in {
    val list = List(1, 2, 3, 4, 5, 6)
    val result = 1
    ListEx.elementAt(list, 0) shouldEqual result
  }

  "22. elementAt(List(1,2,3,4,5,6),1)" should "1" in {
    val list = List(1, 2, 3, 4, 5, 6)
    val result = 1
    ListEx.elementAt(list, 0) shouldEqual result
  }

  "23. mylenght(List(1,2,3,4,5,6))" should "6" in {
    val list = List(1, 2, 3, 4, 5, 6)
    val result = 6
    ListEx.mylenght(list) shouldEqual result
  }

  "24. mylenght(List())" should "0" in {
    val list = List()
    val result = 0
    ListEx.mylenght(list) shouldEqual result
  }

  "25. mylenght(List())" should "1" in {
    val list = List(1)
    val result = 1
    ListEx.mylenght(list) shouldEqual result
  }


  "26. mylenghtL(List(1,2,3,4,5,6))" should "6" in {
    val list = List(1, 2, 3, 4, 5, 6)
    val result = 6
    ListEx.mylenghtL(list) shouldEqual result
  }

  "27. mylenghtL(List())" should "0" in {
    val list = List()
    val result = 0
    ListEx.mylenghtL(list) shouldEqual result
  }

  "28. mylenghtL(List())" should "1" in {
    val list = List(1)
    val result = 1
    ListEx.mylenghtL(list) shouldEqual result
  }

  "29. mylenghtL(List(List(List(1)))" should "1" in {
    val list = List(List(List(1)))
    val result = 1
    ListEx.mylenghtL(list) shouldEqual result
  }

  "30. mylenghtR(List(1,2,3,4,5,6))" should "6" in {
    val list = List(1, 2, 3, 4, 5, 6)
    val result = 6
    ListEx.mylenghtR(list) shouldEqual result
  }

  "31. mylenghtR(List())" should "0" in {
    val list = List()
    val result = 0
    ListEx.mylenghtR(list) shouldEqual result
  }

  "32. mylenghtR(List())" should "1" in {
    val list = List(1)
    val result = 1
    ListEx.mylenghtR(list) shouldEqual result
  }

  "33. mylenghtR(List(List(List(1))))" should "1" in {
    val list = List(List(List(1)))
    val result = 1
    ListEx.mylenghtR(list) shouldEqual result
  }

  "34. isPalindrome(List(1,2,3,4,3,2,1))" should "true" in {
    val list = List(1, 2, 3, 4, 3, 2, 1)
    val result = true
    ListEx.isPalindrome(list) shouldEqual result
  }

  "35. isPalindrome(List(1,2,3,4,3,2,1,2))" should "false" in {
    val list = List(1, 2, 3, 4, 3, 2, 1, 2)
    val result = false
    ListEx.isPalindrome(list) shouldEqual result
  }

  "36. isPalindrome(List())" should "false" in {
    val list = List()
    val result = true
    ListEx.isPalindrome(list) shouldEqual result
  }

  "37. isPalindrome(List(1))" should "true" in {
    val list = List()
    val result = true
    ListEx.isPalindrome(list) shouldEqual result
  }

  "38. flatten(Const(List(Elem(2), Const(List(Elem(1), Const(List(Elem(3), Elem(4))), Elem(5))))))" should " List(2,1,3,4,5)" in {
    val list = ListEx.Const(List(ListEx.Elem(2), ListEx.Const(List(ListEx.Elem(1), ListEx.Const(List(ListEx.Elem(3), ListEx.Elem(4))), ListEx.Elem(5)))))
    val result = List(2, 1, 3, 4, 5)
    ListEx.flatten(list) shouldEqual result
  }

  "39. flatten(Const(List(Elem('a'))))" should " List(a)" in {
    val list = ListEx.Const(List(ListEx.Elem("a")))
    val result = List("a")
    ListEx.flatten(list) shouldEqual result
  }

  "40. compress(List(1,1,1,1,2,3,4,4,4))" should " List(1,2,3,4)" in {
    val list = List(1, 1, 1, 1, 2, 3, 4, 4, 4)
    val result = List(1, 2, 3, 4)
    ListEx.compress(list) shouldEqual result
  }

  "41. compress(List('a','a','a','b','b','c','c','d','d'))" should " List('a','b','c,'d')" in {
    val list = List('a', 'a', 'a', 'b', 'b', 'c', 'c', 'd', 'd')
    val result = List('a', 'b', 'c', 'd')
    ListEx.compress(list) shouldEqual result
  }

  "42. pack(List(1,1,1,1,2,3,4,4,4))" should " List(List(1, 1, 1, 1), List(2), List(3), List(4, 4, 4))" in {
    val list = List(1, 1, 1, 1, 2, 3, 4, 4, 4)
    val result = List(List(1, 1, 1, 1), List(2), List(3), List(4, 4, 4))
    ListEx.pack(list) shouldEqual result
  }

  /*"43. myrndSelect((List('a', 'b', 'c', 'd', 'e', 'f'))" should " rndm Elements selected on a list" in {
    val list = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')
    val result = "A"
    ListEx.myrndSelect(list, 4) shouldEqual result
  }

  "44. mydiffSelect(List( 1 , 1 , 1 , 1 , 2 , 3 , 4 , 4 , 4 ))" should " List of random elements between 0 to 49" in {
    val result = "A"
    ListEx.mydiffSelect(6, 49) shouldEqual result
  }

  "45. myRndmPermute(List('a', 'b', 'c', 'd', 'e', 'f')" should " A permutation of a list " in {
    val list = List('a', 'b', 'c', 'd', 'e', 'f')
    val result = "A"
    ListEx.myRndmPermute(list) shouldEqual result
  }

  "46. mypermuteSelect2(List('a', 'b', 'c', 'd', 'e', 'f')" should " A permutation of a list " in {
    val list = List('a', 'b', 'c', 'd', 'e', 'f')
    val result = "A"
    ListEx.myRndmPermute2(list) shouldEqual result
  }
*/

  "47. myCombinations(List('a', 'b', 'c', 'd', 'e', 'f'))" should " result" in {
    val list = List('a', 'b', 'c', 'd', 'e', 'f')
    val result = List(List('a', 'b', 'c'), List('b', 'a', 'c'), List('b', 'c', 'a'),
      List('a', 'c', 'b'), List('c', 'a', 'b'), List('c', 'b', 'a'), List('a', 'b', 'd'),
      List('b', 'a', 'd'), List('b', 'd', 'a'), List('a', 'd', 'b'), List('d', 'a', 'b'),
      List('d', 'b', 'a'), List('a', 'b', 'e'), List('b', 'a', 'e'), List('b', 'e', 'a'),
      List('a', 'e', 'b'), List('e', 'a', 'b'), List('e', 'b', 'a'), List('a', 'b', 'f'),
      List('b', 'a', 'f'), List('b', 'f', 'a'), List('a', 'f', 'b'), List('f', 'a', 'b'),
      List('f', 'b', 'a'))
    ListEx.mycombinations(list, 3) shouldEqual result
  }

  "48. encode(List(1,1,1,1,2,3,4,4,4))" should "  List((4,1), (1,2), (1,3), (3,4))" in {
    val list = List(1, 1, 1, 1, 2, 3, 4, 4, 4)
    val result = List((4, 1), (1, 2), (1, 3), (3, 4))
    ListEx.encode(list) shouldEqual result
  }

  "49. encode(List('a', 'a', 'c', 'c', 'c', 'f','k', 'k','k','k'))" should " List((2,'a'), (3,'c'), (1,'f'), (4,'k')) " in {
    val list = List('a', 'a', 'c', 'c', 'c', 'f', 'k', 'k', 'k', 'k')
    val result = List((2, 'a'), (3, 'c'), (1, 'f'), (4, 'k'))
    ListEx.encode(list) shouldEqual result
  }

  "50. encodeModified(List('a', 'a', 'c', 'c', 'c', 'f','k', 'k','k','k'))" should " List((2,a), (3,c), 'f', (4,k))" in {
    val list = List('a', 'a', 'c', 'c', 'c', 'f', 'k', 'k', 'k', 'k')
    val result = List((2, 'a'), (3, 'c'), 'f', (4, 'k'))
    ListEx.encodeModified(list) shouldEqual result
  }

  "51. encodeModified(List(1, 1, 1, 1, 2, 3, 4, 4, 4))" should " List((4,1), 2, 3, (3,4))" in {
    val list = List(1, 1, 1, 1, 2, 3, 4, 4, 4)
    val result = List((4, 1), 2, 3, (3, 4))
    ListEx.encodeModified(list) shouldEqual result
  }

  "52. decodeModified(List((4, 'a'), 'b', 'c', (3, 'd')))" should "List('a', 'a', 'a', 'a', 'b', 'c', 'd', 'd', 'd')" in {
    val list = List((4, 'a'), 'b', 'c', (3, 'd'))
    val result = List('a', 'a', 'a', 'a', 'b', 'c', 'd', 'd', 'd')
    ListEx.decodeModified(list) shouldEqual result
  }

  "53. decodeModified(List((4, 1), 2, 3, (3, 4)))" should " List(1, 1, 1, 1, 2, 3, 4, 4, 4)" in {
    val list = List((4, 1), 2, 3, (3, 4))
    val result = List(1, 1, 1, 1, 2, 3, 4, 4, 4)
    ListEx.decodeModified(list) shouldEqual result
  }

  "54. encodeDirect(List('a', 'a', 'c', 'c', 'c', 'f','k', 'k','k','k'))" should " List((2,a), (3,c), 'f', (4,k))" in {
    val list = List('a', 'a', 'c', 'c', 'c', 'f', 'k', 'k', 'k', 'k')
    val result = List((2, 'a'), (3, 'c'), 'f', (4, 'k'))
    ListEx.encodeDirect(list) shouldEqual result
  }

  "55. encodeDirect(List(1, 1, 1, 1, 2, 3, 4, 4, 4))" should " List((4,1), 2, 3, (3,4))" in {
    val list = List(1, 1, 1, 1, 2, 3, 4, 4, 4)
    val result = List((4, 1), 2, 3, (3, 4))
    ListEx.encodeDirect(list) shouldEqual result
  }

  "56. dupli(List(1, 1, 1, 1, 2, 3, 4, 4, 4))" should " List(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 3, 3, 4, 4, 4, 4, 4, 4)" in {
    val list = List(1, 1, 1, 1, 2, 3, 4, 4, 4)
    val result = List(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 3, 3, 4, 4, 4, 4, 4, 4)
    ListEx.dupli(list) shouldEqual result
  }

  "57. dupli(List('a','b','c'))" should " List('a', 'a', 'b', 'b', 'c', 'c')" in {
    val list = List('a', 'b', 'c')
    val result = List('a', 'a', 'b', 'b', 'c', 'c')
    ListEx.dupli(list) shouldEqual result
  }

  "58. repli(List(1, 1, 1, 1, 2, 3, 4, 4, 4),2)" should " List(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 3, 3, 4, 4, 4, 4, 4, 4)" in {
    val list = List(1, 1, 1, 1, 2, 3, 4, 4, 4)
    val result = List(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 3, 3, 4, 4, 4, 4, 4, 4)
    ListEx.repli(list, 2) shouldEqual result
  }

  "59. repli(List('a','b','c'),3)" should " List('a', 'a', 'b', 'b', 'c', 'c')" in {
    val list = List('a', 'b', 'c')
    val result = List('a', 'a', 'a', 'b', 'b', 'b', 'c', 'c', 'c')
    ListEx.repli(list, 3) shouldEqual result
  }

  "60. drop(List(1, 1, 1, 1, 2, 3, 4, 4, 4),2)" should "List(1, 1, 2, 4, 4) " in {
    val list = List(1, 1, 1, 1, 2, 3, 4, 4, 4)
    val result = List(1, 1, 2, 4, 4)
    ListEx.drop(list, 2) shouldEqual result
  }

  "61. drop(List(1, 1, 1, 1, 2, 3, 4, 4, 4),3)" should " List(1, 1, 1, 2, 4, 4)  " in {
    val list = List(1, 1, 1, 1, 2, 3, 4, 4, 4)
    val result = List(1, 1, 1, 2, 4, 4)
    ListEx.drop(list, 3) shouldEqual result
  }

  "62. split(List(1, 1, 1, 1, 2, 3, 4, 4, 4),3)" should " (List(1, 1, 1), List(1, 2, 3, 4, 4, 4)) " in {
    val list = List(1, 1, 1, 1, 2, 3, 4, 4, 4)
    val result = (List(1, 1, 1), List(1, 2, 3, 4, 4, 4))
    ListEx.split(3, list) shouldEqual result
  }

  "63. slice(List(1, 1, 1, 1, 2, 3, 4, 4, 4),2,5)" should " List(1, 1, 1, 2)" in {
    val list = List(1, 1, 1, 1, 2, 3, 4, 4, 4)
    val result = List(1, 1, 1, 2)
    ListEx.slice(list, 2, 5) shouldEqual result
  }

  "64. removeAt(List(1, 1, 1, 1, 2, 3, 4, 4, 4),5)" should "List(1, 1, 1, 1, 3, 4, 4, 4)" in {
    val list = List(1, 1, 1, 1, 2, 3, 4, 4, 4)
    val result = List(1, 1, 1, 1, 3, 4, 4, 4)
    ListEx.removeAt(list, 5)._2 shouldEqual result
  }

  "64. insertAt('a',List(1, 1, 1, 1, 2, 3, 4, 4, 4),5)" should " List(1, 1, 1, 1, 'a', 2, 3, 4, 4, 4)" in {
    val list = List(1, 1, 1, 1, 2, 3, 4, 4, 4)
    val result = List(1, 1, 1, 1, 'a', 2, 3, 4, 4, 4)
    ListEx.insertAt('a', list, 5) shouldEqual result
  }

  "64. range(1,10)" should " LList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) " in {
    val result = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    ListEx.range(1, 10) shouldEqual result
  }

}
