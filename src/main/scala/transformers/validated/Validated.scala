package transformers.validated

sealed trait Validated[+E, +A]

object Validated {
  case class Valid[+A](a: A)   extends Validated[Nothing, A]
  case class Invalid[+E](e: E) extends Validated[E, Nothing]
}
