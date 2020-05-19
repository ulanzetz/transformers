package transformers.validated

case class NEL[+A](head: A, tail: List[A] = Nil)

object NEL {
  def combine[A](x: NEL[A], y: NEL[A]): NEL[A] =
    NEL(x.head, x.tail ::: (y.head :: y.tail))

  def map[A, B](fa: NEL[A])(f: A => B): NEL[B] =
    NEL(f(fa.head), fa.tail.map(f))

  def of[A](head: A, tail: A*): NEL[A] =
    NEL(head, tail.toList)
}
