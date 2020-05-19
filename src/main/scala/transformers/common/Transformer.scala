package transformers.common

trait Transformer[F[_], From, To] {
  def apply(from: From): F[To]
}

object Transformer {
  implicit def idT[F[_]: Applicative, A]: Transformer[F, A, A] =
    Applicative[F].pure[A]

  implicit def listT[F[_], E, A, B](
    implicit
    ts: TransformationSupport[F, E],
    underlying: Transformer[F, A, B]
  ): Transformer[F, List[A], List[B]] =
    _.zipWithIndex.foldRight(ts.applicative.pure(List.empty[B])) {
      case ((element, index), acc) =>
        ts.applicative.map(
          ts.applicative.product(acc, ts.errorSupport.addPath(underlying(element), ErrorPathNode.Index(index)))
        ) { case (acc, next) => next :: acc }
    }

  implicit def mapT[F[_], E, K1 <: AnyRef, V1, K2, V2](
    implicit
    ts: TransformationSupport[F, E],
    keyT: Transformer[F, K1, K2],
    valueT: Transformer[F, V1, V2]
  ): Transformer[F, Map[K1, V1], Map[K2, V2]] =
    _.toList.foldRight(ts.applicative.pure(Map.empty[K2, V2])) {
      case ((key, value), acc) =>
        ts.applicative.map(
          ts.applicative.product(
            acc,
            ts.applicative.product(
              ts.errorSupport.addPath(keyT(key), ErrorPathNode.MapKey(key)),
              ts.errorSupport.addPath(valueT(value), ErrorPathNode.MapValue(key))
            )
          )
        ) {
          case (acc, (key, value)) => acc + (key -> value)
        }
    }
}
