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

      type Pd[C, J] = Di[P]#Aux[C, J]
      type Qd[C, J] = Di[Q]#Aux[C, J]

      type Aux[X, Y] =
        Sum2[Prod2[Pd, Jokers[Q, ?, ?], ?, ?], Prod2[Clowns[P, ?, ?], Qd, ?, ?], X, Y]

      private def mindp[C, J](pj: Either[(J, Pd[C, J]), Prod1[P, Q, C]],
                              qj: _):
        Either[(J, Aux[C, J]), Prod1[P, Q, C]] =
        ???
      private def mindq[C, J](pc: _,
                              qc: Either[(J, Qd[C, J]), Prod1[P, Q, C]]):
        Either[(J, Aux[C, J]), Prod1[P, Q, C]] =
        ???

      def right[C, J](in: Either[Prod1[P, Q, J], (Aux[C, J], C)]): Either[(J, Aux[C, J]), Prod1[P, Q, C]] =
        in match {
          case Left(Prod1(pj, qj)) => ???
          case Right((InL2(Prod2(pd, Jokers(qj))), c)) => ???
          case Right((InR2(Prod2(Clowns(pc), qd)), c)) => ???
        }

      def plug[X](aux: Aux[X, X], x: X): Prod1[P, Q, X] =
        aux match {
          case InL2(Prod2(pd, Jokers(qx))) => Prod1(Di[P].plug(pd, x), qx)
          case InR2(Prod2(Clowns(px), qd)) => Prod1(px, Di[Q].plug(qd, x))
        }
    }

}
