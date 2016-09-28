package dissections.kit

import dissections.{Bifunctor, Functor}

import scala.language.higherKinds

case class Clowns[F[_], A, B](f: F[A])

object Clowns {

  implicit def dissectionsClownIsBifunctor[F[_]: Functor] =
    new Bifunctor[Clowns[F, ?, ?]] {
      def bimap[A, B, C, D](f: A => B, g: C => D)(fa: Clowns[F, A, C]): Clowns[F, B, D] =
        Clowns[F, B, D](Functor[F].fmap(f)(fa.f))
    }

}
