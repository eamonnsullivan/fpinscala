package fpinscala.errorhandling

import org.scalatest.{FlatSpec, MustMatchers}

class OptionSpec extends FlatSpec with MustMatchers {

  val Eps = 1e-3

  "Option.map" should "increment the value" in {
    val result = Some(1) map (_ + 1)
    result mustBe Some(2)
  }

  it should "handle None input" in {
    val input = None
    input map( x => 1) mustBe None
  }

  "Option.getOrElse" should "return real value on Some(input)" in {
    Some(2) getOrElse 3 mustBe 2
  }

  it should "return default value for None input" in {
    None getOrElse 2 mustBe 2
  }

  "Option.flatMap" should "return value as option" in {
    Some(2) flatMap(x => Some(x.toString)) mustBe Some("2")
  }

  it should "handle None input" in {
    None flatMap(x => Some(1)) mustBe None
  }

  "Option.orElse" should "return real value on Some input" in {
    Some(2) orElse Some(3) mustBe Some(2)
  }

  it should "handle None input by returning the else" in {
    None orElse Some(3) mustBe Some(3)
  }

  "Option.filter" should "handle Some input that matches" in {
    Some(2) filter (x => x % 2 == 0) mustBe Some(2)
  }

  it should "handle Some input that fails to match" in {
    Some(2) filter (x => x % 2 != 0) mustBe None
  }

  "Option.variance" should "return something when there is input" in {
    Option.variance(Seq(2.3, 4.5, 6.7, 7.8, 9.9)) map ( x => x mustBe 6.918 +- Eps)
  }

  it should "return None on empty sequence" in {
    Option.variance(Seq()) map (x => x mustBe None)
  }

  "Option.map2" should "process two Some values" in {
    Option.map2(Some(1), Some(2))((a,b) => a + b) mustBe Some(3)
  }

  it should "return None if one is None" in {
    Option.map2(Some(1), None)((a,b) => a + b) mustBe None
  }

  it should "return None if both are None" in {
    Option.map2(None, None)((a,b) => a.toString + b.toString) mustBe None
  }
}
