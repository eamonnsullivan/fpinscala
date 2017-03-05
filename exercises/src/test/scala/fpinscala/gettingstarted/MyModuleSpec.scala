package fpinscala.gettingstarted

import org.scalatest.{FlatSpec, MustMatchers}

class MyModuleSpec extends FlatSpec with MustMatchers {

  "MyModule.fib" should "return 0 when asked for the first fib number" in {
    val result = MyModule.fib(0)
    result mustBe 0
  }

  it should "return 1 when asked for the second fib number" in {
    val result = MyModule.fib(1)
    result mustBe 1
  }

  it should "return 5 when asked for the sixth fib number" in {
    val result = MyModule.fib(5)
    result mustBe 5
  }

  "PolymorphicFunctions.isSorted" should "return true for Array(1, 2, 3) with comparison function (_ < _)" in {
    val result = PolymorphicFunctions.isSorted[Int](Array(1, 2, 3), (_ < _))
    result mustBe true
  }

  it should "return false for Array(1, 2, 3) with comparison function (_ > _)" in {
    val result = PolymorphicFunctions.isSorted[Int](Array(1, 2, 3), (_ > _))
    result mustBe false
  }

  it should "handle strings in of the sort Array('apple', 'banana', 'pear')" in {
    val result = PolymorphicFunctions.isSorted[String](Array("apple", "banana", "pear"), (_ < _))
    result mustBe true
  }

  "PolymorphicFunctions.curry" should "return true for five-character string" in {
    val func = PolymorphicFunctions.curry[String, Int, Boolean]((s, i) => s.length == i)
    func("Apple")(5) mustBe true
  }

  it should "return false for a six character string" in {
    val func = PolymorphicFunctions.curry[String, Int, Boolean]((s, i) => s.length == i)
    func("Apples")(5) mustBe false
  }

  "PolymorphicFunctions.uncurry" should "return a two-parameter function" in {
    val func = PolymorphicFunctions.curry[String, Int, Boolean]((s, i) => s.length == i)
    val func2 = PolymorphicFunctions.uncurry(func)
    func2("Apple", 5) mustBe true
  }

  "PolymorphicFunctions.compose" should "return true for five-character string" in {
    val result = PolymorphicFunctions.compose[String, Int, Boolean]((x) => x == 5, (s) => s.length)
    result("Apple") mustBe true
  }
}
