package fpinscala.datastructures

import org.scalatest.{FlatSpec, MustMatchers}

class ListSpec extends FlatSpec with MustMatchers {

  "List.tail" should "return List(2, 3)" in {
    val result = List.tail(List(1, 2, 3))
    result mustBe List(2, 3)
  }

  it should "return Nil" in {
    val result = List.tail(List(1))
    result mustBe Nil
  }

  it should "throw an exception on empty lists" in {
    assertThrows[Exception] {
      List.tail(List())
    }
  }

  "List.setHead" should "replace the head item" in {
    val result = List.setHead(List("apple", "banana", "pear"), "custard")
    result mustBe List("custard", "banana", "pear")
  }

  it should "return a one-item list when called on Nil" in {
    val result = List.setHead(List(), "custard")
    result mustBe List("custard")
  }

  "List.drop" should "return List(2, 3) when called List(1, 2, 3), 1" in {
    val result = List.drop(List(1, 2, 3), 1)
    result mustBe List(2, 3)
  }

  it should "return Nil when called as List(1, 2, 3), 4" in {
    val result = List.drop(List(1, 2, 3), 4)
    result mustBe Nil
  }

  "List.dropWhile" should "return List(3, 4, 5) when called with List(1, 2, 3, 4, 5), (_ < 3)" in {
    val result = List.dropWhile[Int](List(1, 2, 3, 4, 5), (_ < 3))
    result mustBe List(3, 4, 5)
  }

  it should "return empty list when called with List(1, 2, 3), (_ < 4)" in {
    val result = List.dropWhile[Int](List(1, 2, 3), (_ < 4))
    result mustBe Nil
  }

  it should "return all when called with List('a', 'b', 'c'), (_ < 'a')" in {
    val result = List.dropWhile[Char](List('a', 'b', 'c'), (_ < 'a'))
    result mustBe List('a', 'b', 'c')
  }

  "List.dropWhile2" should "return List(3, 4, 5) when called with List(1, 2, 3, 4, 5)(_< 3)" in {
    val result = List.dropWhile2(List(1, 2, 3, 4, 5))(_ < 3)
    result mustBe List(3, 4, 5)
  }

  it should "return all when called with List('a', 'b', 'c')(_ < 'a')" in {
    val result = List.dropWhile2(List('a', 'b', 'c'))(_ < 'a')
    result mustBe List('a', 'b', 'c')
  }

  "List.init" should "return List(1, 2) for List(1, 2, 3)" in {
    val result = List.init(List(1, 2, 3))
    result mustBe List(1, 2)
  }

  it should "thrown an exception for an empty list" in {
    assertThrows[Exception] {
      List.init(List())
    }
  }

  it should "return Nil for a list with one item" in {
    val result = List.init(List('a'))
    result mustBe Nil
  }

  "List.length" should "return 5 for List(1, 2, 3, 4, 5)" in {
    val result = List.length(List(1, 2, 3, 4, 5))
    result mustBe 5
  }

  it should "return zero for an empty list" in {
    val result = List.length(List())
    result mustBe 0
  }

  it should "return 3 for List(1, 2, 3)" in {
    val result = List.length(List(1, 2, 3))
    result mustBe 3
  }

  "List.foldLeft" should "return correct answers when used to implement length" in {
    val len1 = List.foldLeft(List(1, 2, 3), 0)((x, _) => x + 1)
    val len2 = List.foldLeft(Nil, 0)((x, _) => x + 1)
    val len3 = List.foldLeft(List(1, 2, 3, 4, 5), 0)((x, _) => x + 1)

    len1 mustBe 3
    len2 mustBe 0
    len3 mustBe 5
  }

  it should "return total length of strings when given list of strings" in {
    val result = List.foldLeft(List("apple", "banana", "pear"), 0)((x,s) => x + s.length)
    result mustBe 15
  }

  "List.foldRight" should "return List(1, 2, 3)" in {
    val result = List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
    result mustBe List(1,2,3)
  }

  "List.sum3" should "return 9 for List(1, 3, 5)" in {
    val result = List.sum3(List(1, 3, 5))
    result mustBe 9
  }

  it should "return 0 for List(1, -1)" in {
    val result = List.sum3(List(1, -1))
    result mustBe 0
  }

  "List.product3" should "return 15 for List(1, 3, 5)" in {
    val result = List.product3(List(1.0, 3.0, 5.0))
    result mustBe 15.0
  }

  it should "return 0.0 for List(1, 3, 5, 0, 17, 21)" in {
    val result = List.product3(List(1.0, 3.0, 5.0, 0.0, 17.0, 21.0))
    result mustBe 0.0
  }

  "List.reverse" should "return List(3,2,1) for List(1,2,3)" in {
    val result = List.reverse(List(1,2,3))
    result mustBe List(3,2,1)
  }

  it should "return List('Pear','Banana','Apple)" in {
    val result = List.reverse(List("Apple", "Banana", "Pear"))
    result mustBe List("Pear", "Banana", "Apple")
  }

  "List.append2" should "return List(1,2,3,4,5,6) from two lists" in {
    val result = List.append2(List(1,2,3), List(4,5,6))
    result mustBe List(1,2,3,4,5,6)
  }

  "List.concat" should "return List(1,2,3,4,5,6) from List of Lists" in {
    val result = List.concat(List(List(1,2,3), List(4,5,6)))
    result mustBe List(1,2,3,4,5,6)
  }

  "List.map" should "return List(2,3,4) from List(1,2,3)" in {
    val result = List.map(List(1,2,3))(_ + 1)
    result mustBe List(2,3,4)
  }

  "List.filter" should "return a list of even numbers" in {
    val result = List.filter(List(1,2,3,4,5,6,7))(_ % 2 == 0)
    result mustBe List(2,4,6)
  }

  it should "return odd numbers" in {
    val result = List.filter(List(1,2,3,4,5,6,7))(_ % 2 != 0)
    result mustBe List(1,3,5,7)
  }

  "List.flatMap" should "return List(1,1,2,2,3,3)" in {
    val result = List.flatMap(List(1,2,3))(i => List(i,i))
    result mustBe List(1,1,2,2,3,3)
  }

  "List.filter2" should "still return a list of even numbers" in {
    val result = List.filter(List(1,2,3,4,5,6,7))(_ % 2 == 0)
    result mustBe List(2,4,6)
  }

  it should "still return odd numbers" in {
    val result = List.filter(List(1,2,3,4,5,6,7))(_ % 2 != 0)
    result mustBe List(1,3,5,7)
  }

  "List.zipWith" should "return List(5,7,9)" in {
    val result = List.zipWith(List(1,2,3), List(4,5,6))((x,y) => x + y)
    result mustBe List(5,7,9)
  }

  it should "handle strings" in {
    val result = List.zipWith(List("apple", "banana"), List("pear", "cream"))((x,y) => x + y)
    result mustBe List("applepear", "bananacream")
  }

  it should "be able to handle other operators" in {
    val result = List.zipWith(List(1,2,3), List(4,5,6))((x,y) => x * y)
    result mustBe List(4,10,18)
  }

  it should "handle when one list is shorter than the other" in {
    val result1 = List.zipWith(List(1,2,3,4), List(5,6,7))((x,y) => x + y)
    val result2 = List.zipWith(List(1,2,3), List(4,5,6,7))((x,y) => x + y)

    result1 mustBe List(6,8,10)
    result2 mustBe List(5,7,9)
  }

  "List.hasSubsequence" should "find a one-element subsequence" in {
    List.hasSubsequence(List(1,2,3,4), List(1)) mustBe true
  }

  it should "find a subsequence equal to the super list" in {
    List.hasSubsequence(List(1,2,3,4), List(1,2,3,4)) mustBe true
  }

  it should "fail to find a subsequence with a different order" in {
    List.hasSubsequence(List(1,2,3,4), List(4,3)) mustBe false
  }

  it should "find a subsequence with one less than the super list" in {
    List.hasSubsequence(List(1,2,3,4), List(1,2,3)) mustBe true
  }

  it should "handle text elements" in {
    List.hasSubsequence(List("apple", "pear", "banana"), List("pear", "banana")) mustBe true
  }

  it should "handle a longer list" in {
    val sup = List(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)
    val sub = List(15,16,17,18,19,20)
    List.hasSubsequence(sup, sub) mustBe true
  }

  it should "handle a transposition in the middle" in {
    val sup = List(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)
    val sub = List(15,17,16,18,19,20)
    List.hasSubsequence(sup, sub) mustBe false
  }
}
