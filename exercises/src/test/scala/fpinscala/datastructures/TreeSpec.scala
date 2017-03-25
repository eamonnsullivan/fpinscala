package fpinscala.datastructures

import org.scalatest.{FlatSpec, MustMatchers}

class TreeSpec extends FlatSpec with MustMatchers {

  "Tree.size" should "handle a tree with just a leaf" in {
    Tree.size(Leaf(Nil)) mustBe 1
  }

  it should "handle several branches" in {
    val input = Branch(Leaf(1),Branch(Branch(Leaf(2), Leaf(3)), Branch(Leaf(4), Leaf(5))))
    Tree.size(input) mustBe 9
  }

  it should "handle only leaves" in {
    val input = Branch(Leaf(1), Leaf(2))
    Tree.size(input) mustBe 3
  }

  it should "handle a single leaf" in {
    Tree.size(Leaf(1)) mustBe 1
  }

  "Tree.maximum" should "find the maximum value when it's at the top level" in {
    val input = Branch(Leaf(1), Leaf(2))
    Tree.maximum(input) mustBe 2
  }

  it should "find the maximum a few levels down" in {
    val input = Branch(Leaf(1),Branch(Branch(Leaf(2), Leaf(3)), Branch(Leaf(4), Leaf(5))))
    Tree.maximum(input) mustBe 5
  }

  "Tree.depth" should "handle a single-leaf tree" in {
    val input = Leaf(1)
    Tree.depth(input) mustBe 1
  }

  it should "handle branching" in {
    val input = Branch(Leaf(1), Leaf(2))
    Tree.depth(input) mustBe 2
  }

  it should "handle a more complex example" in {
    val input = Branch(Leaf(1),Branch(Branch(Leaf(2), Leaf(3)), Branch(Leaf(4), Leaf(5))))
    Tree.depth(input) mustBe 4
  }

  "Tree.map" should "increment every leaf" in {
    val input = Branch(Leaf(1),Branch(Branch(Leaf(2), Leaf(3)), Branch(Leaf(4), Leaf(5))))
    Tree.map(input)(x => x + 1) mustBe Branch(Leaf(2),Branch(Branch(Leaf(3), Leaf(4)), Branch(Leaf(5), Leaf(6))))
  }

  it should "handle only leaves" in {
    val input = Leaf(1)
    Tree.map(input)(x => x + 1) mustBe Leaf(2)
  }
}
