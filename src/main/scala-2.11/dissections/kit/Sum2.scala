package dissections.kit

import dissections.Bifunctor

import scala.language.higherKinds

sealed trait Sum2[P[_, _], Q[_, _], X, Y]

case class InL2[P[_, _], Q[_, _], X, Y](p: P[X, Y]) extends Sum2[P, Q, X, Y]
case class InR2[P[_, _], Q[_, _], X, Y](q: Q[X, Y]) extends Sum2[P, Q, X, Y]

object Sum2 {

  implicit def dissectionsSum2IsBifunctor[P[_, _]: Bifunctor, Q[_, _]: Bifunctor] =
    new Bifunctor[Sum2[P, Q, ?, ?]] {
      def bimap[A, B, C, D](f: A => B, g: C => D)(fa: Sum2[P, Q, A, C]): Sum2[P, Q, B, D] =
        fa match {
          case InL2(p) => InL2[P, Q, B, D](Bifunctor[P].bimap(f, g)(p))
          case InR2(q) => InR2[P, Q, B, D](Bifunctor[Q].bimap(f, g)(q))
        }
    }

}
