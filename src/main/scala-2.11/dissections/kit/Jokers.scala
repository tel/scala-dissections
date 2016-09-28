package dissections.kit

import dissections.{Bifunctor, Functor}

import scala.language.higherKinds

case class Jokers[F[_], A, B](f: F[B])

object Jokers {

  implicit def dissectionsJokerIsBifunctor[F[_]: Functor] =
    new Bifunctor[Jokers[F, ?, ?]] {
      def bimap[A, B, C, D](f: A => B, g: C => D)(fa: Jokers[F, A, C]): Jokers[F, B, D] =
        Jokers[F, B, D](Functor[F].fmap(g)(fa.f))
    }

}