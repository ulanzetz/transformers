package transformers.macros

import transformers.common.{Applicative, Transformer}

trait DependentTransformer[F[_], OutFrom, OutTo] {
  type InFrom
  type InTo

  def apply(inner: InFrom => F[InTo]): Transformer[F, OutFrom, OutTo]
}

object DependentTransformer {
  type Aux[F[_], InFrom0, InTo0, OutFrom, OutTo] = DependentTransformer[F, OutFrom, OutTo] {
    type InFrom = InFrom0
    type InTo   = InTo0
  }

  def build[F[_]: Applicative, InFrom0, InTo0, OutFrom, OutTo](
    func: OutFrom => Transformer[F, InFrom0, InTo0] => F[OutTo]
  ): DependentTransformer.Aux[F, InFrom0, InTo0, OutFrom, OutTo] =
    new DependentTransformer[F, OutFrom, OutTo] {
      type InFrom = InFrom0
      type InTo   = InTo0

      def apply(inner: InFrom0 => F[InTo0]): Transformer[F, OutFrom, OutTo] =
        new Transformer[F, OutFrom, OutTo] {
          override def apply(from: OutFrom): F[OutTo] =
            func(from)(new Transformer[F, InFrom0, InTo0] {
              override def apply(from: InFrom0): F[InTo0] = inner(from)
            })
        }
    }
}
