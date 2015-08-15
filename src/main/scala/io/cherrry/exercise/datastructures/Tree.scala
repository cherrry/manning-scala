package io.cherrry.exercise.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // Ex 3.25
  /*
  def size[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }
  }
  */

  // Ex 3.26
  /*
  def max[A](t: Tree[Int]): Int = {
    t match {
      case Leaf(x) => x
      case Branch(left, right) => max(left).max(max(right))
    }
  }
  */

  // Ex 3.27
  /*
  def depth[A](t: Tree[Int]): Int = {
    t match {
      case Leaf(x) => 0
      case Branch(left, right) => 1 + depth(left).max(depth(right))
    }
  }
  */

  // Ex 3.28
  /*
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case Leaf(x) => Leaf(f(x))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }
  }
  */

  // Ex 3.29
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = {
    t match {
      case Leaf(x) => f(x)
      case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
    }
  }

  def size[A](t: Tree[A]): Int = {
    fold(t) { _ => 1 } { 1 + _ + _ }
  }

  def max[A](t: Tree[Int]): Int = {
    fold(t) { x => x } { _ max _ }
  }

  def depth[A](t: Tree[Int]): Int = {
    fold(t) { _ => 0 } { 1 + _.max(_) }
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    fold(t) { x => Leaf(f(x)): Tree[B] } { Branch(_, _) }
  }
}
