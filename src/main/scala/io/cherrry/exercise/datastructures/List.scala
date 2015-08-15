package io.cherrry.exercise.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  // Ex 3.2
  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, t) => t
    }
  }

  // Ex 3.3
  def setHead[A](h: A, t: List[A]): List[A] = Cons(h, t)

  // Ex 3.4
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }
  }

  // Ex 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }
  }

  // Ex 3.6
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  // Ex 3.7
  /*
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  def product(ds: List[Double]): Double = {
    ds match {
      case Cons(0.0, _) => 0.0
      case _ => foldRight(ds, 1.0)(_ * _)
    }
  }
  */


  // Ex 3.9
  def length[A](as: List[A]): Int = {
    foldRight(as, 0) { (_, n) => 1 + n }
  }

  // Ex 3.10
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  // Ex 3.11
  def sum(ns: List[Int]): Int = {
    foldLeft(ns, 0)(_ + _)
  }

  def product(ds: List[Double]): Double = {
    foldLeft(ds, 1.0)(_ * _)
  }

  // Ex 3.12
  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, Nil: List[A]) { (l, e) => Cons(e, l) }
  }

  // Ex 3.13
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as), z) { (a, b) => f(b, a) }
  }

  // Ex 3.14
  def append[A](as: List[A], a: A): List[A] = {
    foldRight(as, Cons(a, Nil)) { Cons(_, _) }
  }

  // Ex 3.15
  def concat[A](as: List[A], bs: List[A]): List[A] = {
    foldRight(as, bs) { Cons(_, _) }
  }

  // Ex 3.16
  def addOne(ns: List[Int]): List[Int] = {
    foldRight(ns, Nil: List[Int]) { (x, xs) => Cons(x + 1, xs) }
  }

  // Ex 3.17
  def toString(ns: List[Double]): List[String] = {
    foldRight(ns, Nil: List[String]) { (x, xs) => Cons(x.toString, xs) }
  }

  // Ex 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, Nil: List[B]) { (x, xs) => Cons(f(x), xs) }
  }

  // Ex 3.19
  /*
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    as match {
      case Nil => Nil
      case Cons(h, t) =>
        if (f(h)) Cons(h, filter(t)(f))
        else filter(t)(f)
    }
  }
  */

  // Ex 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    foldRight(as, Nil: List[B]) { (x, xs) => concat(f(x), xs) }
  }

  // Ex 3.21
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as) {
      case x if f(x) => Cons(x, Nil)
      case _ => Nil
    }
  }

  // Ex 3.22
  def addPair(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(ha, ta), Cons(hb, tb)) => Cons(ha + hb, addPair(ta, tb))
  }

  // Ex 3.23
  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(ha, ta), Cons(hb, tb)) => Cons(f(ha, hb), zipWith(ta, tb)(f))
  }
}
