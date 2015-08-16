package io.cherrry.exercise.errorhandling

import org.scalatest.{Matchers, FlatSpec}

class EitherSpec extends FlatSpec with Matchers {
  it should "convert list of either with some error to error list" in {
    val a = List(Right(1), Right(2), Left("something's wrong"), Right(4))
    Either.sequence(a) should === (Left("something's wrong"))
  }

  it should "convert list of option to option of list" in {
    val a = List(Right(1), Right(2), Right(3))
    Either.sequence(a) should === (Right(List(1, 2, 3)))
  }
}
