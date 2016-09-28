package dissections

import scala.language.higherKinds

trait Functor[F[_]] {

  def fmap[A, B](f: A => B)(fa: F[A]): F[B]

}

object Functor {

  def apply[T[_]](implicit F: Functor[T]): Functor[T] = F

  /**
    * A `Functor` with `Nothing` inside can be considered a `Functor` with
    * anything inside. Produces the result without traversing the structure.
    */
  def inflate[F[_]: Functor, A](f: F[Nothing]): F[A] =
    f.asInstanceOf[F[A]]

}
