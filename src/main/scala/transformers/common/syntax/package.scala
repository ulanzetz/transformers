package transformers.common

package object syntax {
  implicit class TransformerSyntax[A](private val a: A) extends AnyVal {
    def transformTo[F[_], B](implicit transformer: Transformer[F, A, B]): F[B] =
      transformer(a)
  }
}
