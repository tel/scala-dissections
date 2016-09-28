package dissections.kit

import dissections.{Di, Functor}

import scala.language.higherKinds

case class Prod1[P[_], Q[_], X](p: P[X], q: Q[X])

object Prod1 {

  implicit def dissectionsProd1IsFunctor[P[_]: Functor, Q[_]: Functor] =
    new Functor[Prod1[P, Q, ?]] {
      def fmap[A, B](f: A => B)(fa: Prod1[P, Q, A]): Prod1[P, Q, B] =
        fa match {
          case Prod1(p, q) => Prod1(
            p = Functor[P].fmap(f)(p),
            q = Functor[Q].fmap(f)(q)
          )
        }
    }

  implicit def dissectionsProd1Diss[P[_]: Di, Q[_]: Di] =
    new Di[Prod1[P, Q, ?]] {
      type Aux[X, Y] =
        Sum2[
          Prod2[Di[P]#Aux, Jokers[Q, ?, ?], ?, ?],
          Prod2[Clowns[P, ?, ?], Di[Q]#Aux, ?, ?],
          X, Y]
    }

}
