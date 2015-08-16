package io.cherrry.exercise.errorhandling

sealed trait Option[+A] {
  // Ex 4.1
  def map[B](f: A => B): Option[B] = this match {
    case Some(x) => Some(f(x))
    case _ => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f).getOrElse(None)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(x) => x
    case _ => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    map(Some(_)).getOrElse(ob)
  }

  def filter(f: A => Boolean): Option[A] = {
    flatMap(a => if (f(a)) Some(a) else None)
  }
}

case object None extends Option[Nothing]
case class Some[A](get: A) extends Option[A]

object Option {
  // Ex 4.2
  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap { m =>
      mean(xs.map { x => math.pow(x - m, 2) })
    }
  }

  // Ex 4.3
  /*
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Some(xa), Some(xb)) => Some(f(xa, xb))
    case _ => None
  }
  */
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)
  }

  // Ex 4.4
  /*
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldLeft(Some(Nil): Option[List[A]]) { map2(_, _)(_.::(_)) }
  }
  */

  // Ex 4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldLeft(Some(Nil): Option[List[B]]) { (l, e) => map2(l, f(e)) { (list, elem) => list ++ List(elem) } }
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    traverse(a) { x => x }
  }
}
