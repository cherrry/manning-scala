package io.cherrry.exercise.errorhandling

sealed trait Either[+E, +A] {
  // Ex 4.6
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case Left(e) => Left(e)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => Right(a)
    case Left(_) => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    for {
      a <- this
      b1 <- b
    } yield f(a, b1)
  }
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  // Ex 4.7
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    traverse(es) { x => x }
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    as.foldLeft(Right(Nil): Either[E, List[B]]) { (ei, x) => ei.map2(f(x)) { (list, elem) => list ++ List(elem) } }
  }
}
