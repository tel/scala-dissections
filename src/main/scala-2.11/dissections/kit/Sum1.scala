package dissections.kit

import dissections.{Di, Functor}

import scala.language.higherKinds

sealed trait Sum1[P[_], Q[_], X]
case class InL1[P[_], Q[_], X](p: P[X]) extends Sum1[P, Q, X]
case class InR1[P[_], Q[_], X](q: Q[X]) extends Sum1[P, Q, X]

object Sum1 {

  implicit def dissectionsSum1IsFunctor[P[_]: Functor, Q[_]: Functor, X] =
    new Functor[Sum1[P, Q, ?]] {
      def fmap[A, B](f: A => B)(fa: Sum1[P, Q, A]): Sum1[P, Q, B] =
        fa match {
          case InL1(p) => InL1[P, Q, B](Functor[P].fmap(f)(p))
          case InR1(q) => InR1[P, Q, B](Functor[Q].fmap(f)(q))
        }
    }

  implicit def dissectionsSum1Diss[P[_]: Di, Q[_]: Di] =
    new Di[Sum1[P, Q, ?]] {
      type Aux[X, Y] = Sum2[Di[P]#Aux, Di[Q]#Aux, X, Y]
    }

}