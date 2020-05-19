package transformers.macros

import transformers.common.{TransformationSupport, Transformer}

import scala.language.experimental.macros

trait MacroDerivation {
  implicit def materializeTransformer[F[_], E, From, To](
    implicit
    ts: TransformationSupport[F, E]
  ): Transformer[F, From, To] =
    macro TransformerMacro.materializeTransformer[F, From, To]
}
