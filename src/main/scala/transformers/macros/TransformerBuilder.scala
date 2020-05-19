package transformers.macros

import transformers.common.TransformationSupport

import scala.language.experimental.macros

case class TransformerBuilder[F[_], E, From, To, C <: TransformerConfig](
  from: From,
  ts: TransformationSupport[F, E],
  consts: Map[String, Any]
) {
  def mapping[FromField, ToField](
    fromSel: From => FromField,
    toSel: To => ToField
  ): TransformerBuilder[F, E, From, To, _ <: TransformerConfig] =
    macro TransformerBuilderMacro.mappingImpl[F, E, From, To, C, FromField, ToField]

  def const[ToField](toSel: To => ToField, value: ToField): TransformerBuilder[F, E, From, To, _ <: TransformerConfig] =
    macro TransformerBuilderMacro.constImpl[F, E, From, To, C, ToField]

  def transform: F[To] =
    macro TransformerMacro.buildTransformer[F, From, To, C]
}
