package co.s4n.listex


import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ComprehensionListSpec extends AnyFlatSpec with Matchers {

  "forLenght(List(2, 3, 4, 5, 6, 6, 7, 1, 3, 3))" should "10" in {
    val lst = List(2, 3, 4, 5, 6, 6, 7, 1, 3, 3)
    ComprehensionList.forLength(lst) shouldEqual 10
  }

  "forLenght(List())" should "0" in {
    val lst = List()
    ComprehensionList.forLength(lst) shouldEqual 0
  }

  "forLenght(List(1))" should "1" in {
    val lst = List(1)
    ComprehensionList.forLength(lst) shouldEqual 1
  }

  "forLast(List(2, 3, 4, 5, 6, 6, 7, 1, 3, 3))" should "3" in {
    val lst = List(2, 3, 4, 5, 6, 6, 7, 1, 3, 3)
    ComprehensionList.forLast(lst) shouldEqual 3
  }

  "forLast(List(a,b,c,d))" should "d" in {
    val lst = List("a","b","c","d")
    ComprehensionList.forLast(lst) shouldEqual "d"
  }

  "forLast(List(2))" should "2" in {
    val lst = List(2)
    ComprehensionList.forLast(lst) shouldEqual 2
  }

  "forHead(List(2, 3, 4, 5, 6, 6, 7, 1, 3, 3))" should "2" in {
    val lst = List(2, 3, 4, 5, 6, 6, 7, 1, 3, 3)
    ComprehensionList.forHead(lst) shouldEqual 2
  }

  "forHead(List(a,b,c,d))" should "a" in {
    val lst = List("a","b","c","d")
    ComprehensionList.forHead(lst) shouldEqual "a"
  }

  "forHead(List(2))" should "2" in {
    val lst = List(2)
    ComprehensionList.forHead(lst) shouldEqual 2
  }

  "ComprehensionList.forKElement(3, lst)" should "4" in {
    val lst = List(2, 3, 4, 5, 6, 6, 7, 1)
    ComprehensionList.forKElement(3, lst) shouldEqual 4
  }

  "ComprehensionList.forKElement(1, lst)" should "2" in {
    val lst = List(2, 3, 4, 5, 6, 6, 7, 1)
    ComprehensionList.forKElement(1, lst) shouldEqual 2
  }
}
