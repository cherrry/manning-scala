package io.cherrry.exercise.gettingstarted

object GettingStarted {
  // Ex 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    if (as.length <= 1) true
    else ordered(as(0), as(1)) && isSorted(as.drop(1), ordered)
  }

  // Ex 2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

  // Ex 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  // Ex 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }
}
