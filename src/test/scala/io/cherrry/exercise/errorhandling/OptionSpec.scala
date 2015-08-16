package io.cherrry.exercise.errorhandling

import org.scalatest.{Matchers, FlatSpec}

class OptionSpec extends FlatSpec with Matchers {
  it should "return none for mean of empty list" in {
    val xs = Seq[Double]()
    Option.mean(xs) should === (None)
  }

  it should "return the mean of list" in {
    val xs = Seq(1.0, 2.0, 3.0, 4.0)
    Option.mean(xs) should === (Some(2.5))
  }

  it should "return none for variance of empty list" in {
    val xs = Seq[Double]()
    Option.variance(xs) should === (None)
  }

  it should "return the variance of list" in {
    val xs = Seq(1.0, 2.0, 3.0, 4.0)
    Option.variance(xs) should === (Some(1.25))
  }

  it should "convert list of options with some none to none list" in {
    val a = List(Some(1), Some(2), None, Some(4))
    Option.sequence(a) should === (None)
  }

  it should "convert list of option to option of list" in {
    val a = List(Some(1), Some(2), Some(3))
    Option.sequence(a) should === (Some(List(1, 2, 3)))
  }
}
