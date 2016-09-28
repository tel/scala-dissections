package dissections

package object kit {

  type One1[X] = K1[Unit, X]
  type One2[X, Y] = K2[Unit, X, Y]

  def one1[X]: One1[X] = K1(())
  def one2[X, Y]: One2[X, Y] = K2(())

  type Zero1[X] = K1[Nothing, X]
  type Zero2[X, Y] = K2[Nothing, X, Y]

  object implicits {

    implicit class NothingIsMagic(nothing: Nothing) {
      def magic[A]: A = nothing.asInstanceOf[A]
    }

  }

}
