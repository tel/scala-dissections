package dissections.kit

import dissections.{Di, Functor}

case class Id[X](x: X)

object Id {
  implicit object dissectionsIdIsFunctor extends Functor[Id] {
    def fmap[A, B](f: A => B)(fa: Id[A]): Id[B] = Id(f(fa.x))
  }

  implicit object dissectionsIdDiss extends Di[Id] {
    type Aux[X, Y] = One2[X, Y]

    def right[J, C](in: Either[Id[J], (One2[C, J], C)]): Either[(J, One2[C, J]), Id[C]] =
      in match {
        case Left(Id(j)) => Left[(J, One2[C, J]), Id[C]]((j, one2))
        case Right((one, clown)) => Right(Id(clown))
      }

    def plug[X](aux: One2[X, X], x: X): Id[X] =
      Id(x)
  }
}
