package dissections.kit

import dissections.Bifunctor

import scala.language.higherKinds

case class Prod2[P[_, _], Q[_, _], X, Y](p: P[X, Y], q: Q[X, Y])

object Prod2 {

  implicit def dissectionsProd2IsBifunctor[P[_, _]: Bifunctor, Q[_, _]: Bifunctor] =
    new Bifunctor[Prod2[P, Q, ?, ?]] {
      def bimap[A, B, C, D](f: A => B, g: C => D)(fa: Prod2[P, Q, A, C]): Prod2[P, Q, B, D] =
        fa match {
          case Prod2(p, q) => Prod2(
            p = Bifunctor[P].bimap(f, g)(p),
            q = Bifunctor[Q].bimap(f, g)(q)
          )
        }
    }

}
