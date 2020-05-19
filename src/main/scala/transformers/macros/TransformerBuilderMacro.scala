package transformers.macros

import scala.reflect.macros.whitebox

class TransformerBuilderMacro(val c: whitebox.Context) extends MacroUtils {
  import c.universe._

  def mappingImpl[
    F[_]: WTTF,
    E: WeakTypeTag,
    From: WeakTypeTag,
    To: WeakTypeTag,
    C: WeakTypeTag,
    FromField: WeakTypeTag,
    ToField: WeakTypeTag
  ](fromSel: Tree,
    toSel: Tree
  ): Tree = {
    val F    = WTTF[F]
    val E    = weakTypeOf[E]
    val From = weakTypeOf[From]
    val To   = weakTypeOf[To]
    val C    = weakTypeOf[C]

    val prefix = c.prefix.tree

    val fromFieldName = extractFieldName(fromSel)
    val toFieldName   = extractFieldName(toSel)

    q"""new _root_.transformers.macros.TransformerBuilder[
         $F,
         $E,
         $From,
         $To,
         _root_.transformers.macros.TransformerConfig.Mapping[
           ${constType(fromFieldName)},
           ${constType(toFieldName)},
           $C
         ]
       ]($prefix.from, 
         $prefix.ts, 
         $prefix.consts
         )
    """
  }

  def constImpl[F[_]: WTTF, E: WeakTypeTag, From: WeakTypeTag, To: WeakTypeTag, C: WeakTypeTag, ToField: WeakTypeTag](
    toSel: Tree,
    value: Tree
  ): Tree = {
    val F    = WTTF[F]
    val E    = weakTypeOf[E]
    val From = weakTypeOf[From]
    val To   = weakTypeOf[To]
    val C    = weakTypeOf[C]

    val prefix = c.prefix.tree

    val toFieldName = extractFieldName(toSel)

    q"""new _root_.transformers.macros.TransformerBuilder[
         $F,
         $E,
         $From,
         $To,
         _root_.transformers.macros.TransformerConfig.Const[
           ${constType(toFieldName)},
           $C
         ]
       ]($prefix.from, $prefix.ts, $prefix.consts.updated(${literal(toFieldName)}, $value))
    """
  }
}
