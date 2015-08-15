package io.cherrry.exercise.datastructures

import org.scalatest.{Matchers, FlatSpec}

class TreeSpec extends FlatSpec with Matchers {
  it should "return the size of tree" in {
    val t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    Tree.size(t) should === (5)
  }

  it should "return the maximum value in tree" in {
    val t = Branch(Branch(Leaf(1), Leaf(3)), Branch(Leaf(2), Leaf(4)))
    Tree.max(t) should === (4)
  }

  it should "return the maximum depth of tree" in {
    val t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    Tree.depth(t) should === (2)
  }

  it should "convert each element in tree to string" in {
    val t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    Tree.map(t) { _.toString } should === (Branch(Branch(Leaf("1"), Leaf("2")), Leaf("3")))
  }
}
