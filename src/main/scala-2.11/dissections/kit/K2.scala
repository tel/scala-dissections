package dissections.kit

import dissections.Bifunctor

case class K2[A, X, Y](a: A) {
  def magic[U, V]: K2[A, U, V] = asInstanceOf[K2[A, U, V]]
}

object K2 {

  implicit def dissectionsK2IsBifunctor[X] =
    new Bifunctor[K2[X, ?, ?]] {
      def bimap[A, B, C, D](f: A => B, g: C => D)(fa: K2[X, A, C]): K2[X, B, D] =
        fa.magic
    }

}
