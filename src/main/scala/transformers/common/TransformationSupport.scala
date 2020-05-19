package transformers.common

trait Applicative[F[_]] {
  def pure[A](x: A): F[A]

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]

  def map[A, B](fa: F[A])(f: A => B): F[B]
}

object Applicative {
  @inline
  def apply[F[_]](implicit inst: Applicative[F]): Applicative[F] =
    inst
}

trait ErrorSupport[F[_], E] {
  def addPath[A](fa: F[A], node: ErrorPathNode): F[A]

  def raiseError[A](e: E): F[A]
}

object ErrorSupport {
  @inline
  def apply[F[_], E](implicit inst: ErrorSupport[F, E]): ErrorSupport[F, E] =
    inst
}

trait TransformationSupport[F[_], E] {
  def applicative: Applicative[F]

  def errorSupport: ErrorSupport[F, E]
}

object TransformationSupport {
  @inline
  def apply[F[_], E](implicit inst: TransformationSupport[F, E]): TransformationSupport[F, E] =
    inst
}
