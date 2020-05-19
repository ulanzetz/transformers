package transformers.macros

import scala.reflect.macros.blackbox

trait MacroUtils {
  val c: blackbox.Context

  import c.universe._

  type WTTF[F[_]] = c.WeakTypeTag[F[Unit]]

  object WTTF {
    def apply[F[_]: WTTF]: Type =
      weakTypeOf[F[Unit]].typeConstructor
  }

  def extractFieldName(tree: Tree): Name =
    tree match {
      case q"(${valDef: ValDef}) => ${ident: Ident}.${fieldName: Name}" if valDef.name == ident.name =>
        fieldName
      case _ =>
        c.abort(c.enclosingPosition, "Invalid selector")
    }

  def constType(name: Name): ConstantType =
    c.internal.constantType(const(name))

  def literal(name: Name): Literal =
    Literal(const(name))

  def const(name: Name): Constant =
    Constant(name.decodedName.toString)
}
