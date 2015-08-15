package io.cherrry.exercise.datastructures

import org.scalatest.{Matchers, FlatSpec}

class ListSpec extends FlatSpec with Matchers {
  it should "drop the first two elements in list" in {
    val l = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    val f = { (_: Int) <= 2 }
    List.dropWhile(l, f) should === (Cons(3, Cons(4, Nil)))
  }

  it should "drop the last element in list" in {
    val l = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    List.init(l) should === (Cons(1, Cons(2, Cons(3, Nil))))
  }

  it should "return length = 0 for empty list" in {
    val l = Nil
    List.length(l) should === (0)
  }

  it should "return length of list" in {
    val l = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    List.length(l) should === (4)
  }

  it should "return sum = 0 for empty list" in {
    val l = Nil
    List.sum(l) should === (0)
  }

  it should "sum up all values in list" in {
    val l = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    List.sum(l) should === (10)
  }

  it should "return product = 1.0 for empty list" in {
    val l = Nil
    List.product(l) should === (1.0)
  }

  it should "multiply all values in list" in {
    val l = Cons(1.0, Cons(2.0, Cons(3.0, Nil)))
    List.product(l) should === (6.0)
  }

  it should "reverse the list" in {
    val l = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    List.reverse(l) should === (Cons(4, Cons(3, Cons(2, Cons(1, Nil)))))
  }

  it should "append the last element" in {
    val l = Cons(1, Cons(2, Cons(3, Nil)))
    List.append(l, 4) should === (Cons(1, Cons(2, Cons(3, Cons(4, Nil)))))
  }

  it should "concat two lists" in {
    val l1 = Cons(1, Cons(2, Cons(3, Nil)))
    val l2 = Cons(4, Cons(5, Nil))
    List.concat(l1, l2) should === (Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil))))))
  }

  it should "add one to each element in list" in {
    val l = Cons(1, Cons(2, Cons(3, Nil)))
    List.addOne(l) should === (Cons(2, Cons(3, Cons(4, Nil))))
  }

  it should "convert list of doubles to string" in {
    val l = Cons(1.0, Cons(2.0, Nil))
    List.toString(l) should === (Cons("1.0", Cons("2.0", Nil)))
  }

  it should "double each value in list" in {
    val l = Cons(1, Cons(2, Cons(3, Nil)))
    List.map(l){ _ * 2 } should === (Cons(2, Cons(4, Cons(6, Nil))))
  }

  it should "keeps even values only" in {
    val l = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    List.filter(l) { _ % 2 == 0 } should === (Cons(2, Cons(4, Nil)))
  }

  it should "return the same list with flatMap" in {
    val l = Cons(1, Cons(2, Cons(3, Nil)))
    List.flatMap(l) { Cons(_, Nil ) } should === (l)
  }

  it should "add list elements pairwise" in {
    val a = Cons(1, Cons(2, Cons(3, Nil)))
    val b = Cons(4, Cons(5, Cons(6, Nil)))
    List.addPair(a, b) should === (Cons(5, Cons(7, Cons(9, Nil))))
  }

  it should "add list elements pairwise with zipWith" in {
    val a = Cons(1, Cons(2, Cons(3, Nil)))
    val b = Cons(4, Cons(5, Cons(6, Nil)))
    List.zipWith(a, b) { _ + _ } should === (Cons(5, Cons(7, Cons(9, Nil))))
  }
}
