package dissections.kit

import dissections.{Di, Functor}

case class K1[A, X](a: A) {
  def magic[Y]: K1[A, Y] = asInstanceOf[K1[A, Y]]
}

object K1 {

  import implicits._

  implicit def dissectionsK1IsFunctor[C]: Functor[K1[C, ?]] =
    new Functor[K1[C, ?]] {
      def fmap[A, B](f: A => B)(fa: K1[C, A]): K1[C, B] = fa.magic
    }

  implicit def dissectionsK1Diss[A] =
    new Di[K1[A, ?]] {
      type Aux[X, Y] = Zero2[X, Y]

      def right[J, C](in: Either[K1[A, J], (Zero2[C, J], C)]): Either[(J, Zero2[C, J]), K1[A, C]] =
        in match {
          case Left(k) => Right(k.magic)
          case Right((zero, clown)) => NothingIsMagic(zero.a).magic
        }

      def plug[X](aux: Zero2[X, X], x: X): K1[A, X] =
        NothingIsMagic(aux.a).magic

    }
}
