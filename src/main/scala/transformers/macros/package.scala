package transformers

import transformers.common.TransformationSupport

package object macros extends MacroDerivation {
  implicit class BuilderSyntax[From](private val from: From) extends AnyVal {
    def into[F[_], E, To](
      implicit
      ts: TransformationSupport[F, E]
    ): TransformerBuilder[F, E, From, To, TransformerConfig.Empty] =
      TransformerBuilder(from, ts, Map.empty)
  }
}
