package dissections

import scala.language.higherKinds

/**
  * Like a functor, but twice as nice.
  */
trait Bifunctor[F[_, _]] {

  def bimap[A, B, C, D](f: A => B, g: C => D)(fa: F[A, C]): F[B, D]

}

object Bifunctor {

  def apply[T[_, _]](implicit F: Bifunctor[T]): Bifunctor[T] = F

}
