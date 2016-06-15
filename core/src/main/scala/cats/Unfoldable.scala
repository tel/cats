package cats

import simulacrum.typeclass

@typeclass trait Unfoldable[F[_]] extends Foldable[F] { self =>

  def unfoldRight[A, B](b: B)(f: B => Eval[Option[(A, B)]]): Eval[F[A]]

  def unfoldLeft[A, B](seed: B)(f: B => Option[(B, A)]): F[A]

  def none[A]: F[A] =
    unfoldRight(())(_ => Eval.now(None)).value

  def singleton[A](value: A): F[A] =
    replicate(1, value)

  def replicate[A](n: Int, value: A): F[A] = {
    def go(i: Int): Option[(Int, A)] =
      if(i <= 0) None else Some((i - 1, value))
    unfoldLeft(n)(go)
  }

  def prepend[A](head: A, tail: F[A]): F[A] =
    reverse(throughList(tail)(head +: _))

  def append[A](init: F[A], last: A): F[A] =
    reverse(throughList(init)(_ :+ last))

  def reverse[A](fa: F[A]): F[A] =
    throughList(fa)(identity)

  private def throughList[A](fa: F[A])(f: List[A] => List[A]): F[A] = {
    def go(l: List[A]): Option[(List[A], A)] = l match {
      case Nil     => None
      case x :: xs => Some((xs, x))
    }
    unfoldLeft(f(toList(fa)))(go)
  }

}

object Unfoldable {

}
