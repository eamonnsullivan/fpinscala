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
}
