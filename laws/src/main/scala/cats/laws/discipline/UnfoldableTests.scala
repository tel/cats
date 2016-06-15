package cats
package laws
package discipline

import org.scalacheck.{Prop, Arbitrary}
import Prop.forAll
import cats.std.option._

trait UnfoldableTests[F[_]] extends FoldableTests[F] {
  def laws: UnfoldableLaws[F]

  def unfoldable[A: Arbitrary, B: Arbitrary](implicit
    ArbFA: Arbitrary[F[A]],
    Eqa: Eq[A],
    EqFa: Eq[F[A]],
    B: Monoid[B],
    EqB: Eq[B]
  ): RuleSet =
    new RuleSet {
      def name: String = "unfoldable"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(foldable[A, B])
      def props: Seq[(String, Prop)] = Seq(
        "prependHeadOption" -> forAll(laws.prependHeadOption[A] _),
        "appendLastOption" -> forAll(laws.appendLastOption[A] _),
        "reverseReverseId" -> forAll(laws.reverseReverseId[A] _),
        "reverseHeadOption" -> forAll(laws.reverseHeadOption[A] _)
      )
    }
}


object UnfoldableTests {
  def apply[F[_]: Unfoldable]: UnfoldableTests[F] =
    new UnfoldableTests[F] { def laws: UnfoldableLaws[F] = UnfoldableLaws[F] }
}
