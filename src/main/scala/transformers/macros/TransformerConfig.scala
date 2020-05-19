package transformers.macros

sealed abstract class TransformerConfig

object TransformerConfig {
  class Empty                                                         extends TransformerConfig
  class Const[Name <: String, C <: TransformerConfig]                 extends TransformerConfig
  class Mapping[From <: String, To <: String, C <: TransformerConfig] extends TransformerConfig
}
