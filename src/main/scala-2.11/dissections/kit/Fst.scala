package dissections.kit

import dissections.Bifunctor

case class Fst[X, Y](x: X)

object Fst {

  implicit object dissectionsFstIsBifunctor extends Bifunctor[Fst] {
    def bimap[A, B, C, D](f: A => B, g: C => D)(fa: Fst[A, C]): Fst[B, D] =
      fa match {
        case Fst(x) => Fst(f(x))
      }
  }

}
