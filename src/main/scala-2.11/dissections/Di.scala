package dissections

import scala.language.higherKinds

/**
  * A `Di[T]`, a "dissection" of T, describes how to stop halfway through a
  * traversal of `T`.
  */
abstract class Di[T[_]: Functor] {

  /**
    * The "dissected" version of `T`. A value of `Aux[C, J]` is a frozen
    * traversal of `T` where clowns are on the left of the current position
    * and jokers on the right. If we have a joker, we can insert it and move
    * rightward. If we have a clown we can insert it and move leftward.
    */
  type Aux[C, J]

  /**
    * Any dissected value is a bifunctor.
    */
  val auxIsBifunctor: Bifunctor[Aux] =
    implicitly

  /**
    * Creep gradually toward the right. Given either a `T[J]` of all jokers
    * (the furthest right state) or a dissected `T`, `Aux[C, J]` with some
    * new clown to insert we creep rightward ending up either with one new
    * joker and another dissected state or `T[C]`, the type filled with clowns.
    */
  def right[C, J](in: Either[T[J], (Aux[C, J], C)]): Either[(J, Aux[C, J]), T[C]]

  /**
    * If the jokers and clowns are the same then we can just fill the hole to
    * retrieve our initial `T`.
    */
  def plug[X](aux: Aux[X, X], x: X): T[X]

}

object Di {

  def apply[T[_]](implicit F: Di[T]): Di[T] = F

}
