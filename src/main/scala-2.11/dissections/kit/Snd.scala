package dissections.kit

import dissections.Bifunctor

case class Snd[X, Y](y: Y)

object Snd {

  implicit object dissectionsSndIsBifunctor extends Bifunctor[Snd] {
    def bimap[A, B, C, D](f: A => B, g: C => D)(fa: Snd[A, C]): Snd[B, D] =
      fa match {
        case Snd(x) => Snd(g(x))
      }
  }

}
