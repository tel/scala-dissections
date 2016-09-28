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

      type Pd[X, Y] = Di[P]#Aux[X, Y]
      type Qd[X, Y] = Di[Q]#Aux[X, Y]

      type Aux[X, Y] = Sum2[Pd, Qd, X, Y]

      private def mindp[C, J](x: Either[(J, Pd[C, J]), P[C]]):
        Either[(J, Sum2[Pd, Qd, C, J]), Sum1[P, Q, C]] =
          x match {
            case Left((j, pd)) => Left((j, InL2[Pd, Qd, C, J](pd)))
            case Right(pc) => Right(InL1[P, Q, C](pc))
          }

      private def mindq[C, J](x: Either[(J, Qd[C, J]), Q[C]]):
        Either[(J, Sum2[Pd, Qd, C, J]), Sum1[P, Q, C]] =
          x match {
            case Left((j, qd)) => Left((j, InR2[Pd, Qd, C, J](qd)))
            case Right(qc) => Right(InR1[P, Q, C](qc))
          }

      def right[C, J](in: Either[Sum1[P, Q, J], (Aux[C, J], C)]): Either[(J, Aux[C, J]), Sum1[P, Q, C]] =
        in match {
          case Left(InL1(pj)) => mindp(Di[P].right(Left(pj)))
          case Left(InR1(qj)) => mindq(Di[Q].right(Left(qj)))
          case Right((InL2(pd), c)) => mindp(Di[P].right(Right((pd, c))))
          case Right((InR2(qd), c)) => mindq(Di[Q].right(Right((qd, c))))
        }

      def plug[X](aux: Aux[X, X], x: X): Sum1[P, Q, X] =
        aux match {
          case InL2(pd) => InL1[P, Q, X](Di[P].plug(pd, x))
          case InR2(qd) => InR1[P, Q, X](Di[Q].plug(qd, x))
        }
    }

}