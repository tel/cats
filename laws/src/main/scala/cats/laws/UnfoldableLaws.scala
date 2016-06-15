package cats
package laws

import cats.syntax.foldable._
import cats.syntax.unfoldable._

trait UnfoldableLaws[F[_]] extends FoldableLaws[F] {
  implicit override def F: Unfoldable[F]

  def prependHeadOption[A](fa: F[A], a: A): IsEq[Option[A]] =
    F.prepend(a, fa).headOption <-> Some(a)

  def appendLastOption[A](fa: F[A], a: A): IsEq[Option[A]] =
    fa.append(a).lastOption <-> Some(a)

  def reverseReverseId[A](fa: F[A]): IsEq[F[A]] =
    fa.reverse.reverse <-> fa

  def reverseHeadOption[A](fa: F[A]): IsEq[Option[A]] =
    fa.reverse.headOption <-> fa.lastOption
}

object UnfoldableLaws {
  def apply[F[_]](implicit ev: Unfoldable[F]): UnfoldableLaws[F] =
    new UnfoldableLaws[F] { def F: Unfoldable[F] = ev }
}
