package transformers.macros

import scala.annotation.tailrec
import scala.reflect.macros.blackbox

class TransformerMacro(val c: blackbox.Context) extends MacroUtils {
  import c.universe._

  case class Config(
    from: Type,
    to: Type,
    f: Type,
    ts: Tree,
    consts: Tree = EmptyTree,
    mapping: Map[String, String] = Map.empty,
    constFields: Set[String] = Set.empty
  )

  def buildTransformer[F[_]: WTTF, From: WeakTypeTag, To: WeakTypeTag, C: WeakTypeTag]: c.Tree = {
    val builderName = TermName(c.freshName("builder"))

    val baseCfg =
      Config(weakTypeOf[From], weakTypeOf[To], WTTF[F], q"$builderName.ts", q"$builderName.consts")

    val transformer = deriveTransformer(captureConfig(weakTypeOf[C], baseCfg))

    q"""
       val $builderName = ${c.prefix.tree}
       $transformer.apply($builderName.from)
     """
  }

  def materializeTransformer[F[_]: WTTF, From: WeakTypeTag, To: WeakTypeTag](ts: Tree): c.Tree = {
    val cfg = Config(weakTypeOf[From], weakTypeOf[To], WTTF[F], ts)
    findImplicitTransformer(cfg).getOrElse(deriveTransformer(cfg))
  }

  private def findImplicitTransformer(cfg: Config): Option[Tree] = {
    val constructor = typeOf[transformers.common.Transformer[Tuple1, _, _]].typeConstructor
    val expanded    = constructor.etaExpand
    val tpe         = expanded.finalResultType.substituteTypes(expanded.typeParams, List(cfg.f, cfg.from, cfg.to))

    findImplicit(tpe)
  }

  private def findImplicitDependentTransformer(cfg: Config): Option[Tree] = {
    val constructor = typeOf[transformers.macros.DependentTransformer[Tuple1, _, _]].typeConstructor
    val expanded    = constructor.etaExpand
    val tpe         = expanded.finalResultType.substituteTypes(expanded.typeParams, List(cfg.f, cfg.from, cfg.to))

    findImplicit(tpe).map { dependentTree =>
      val tpe = dependentTree.tpe.finalResultType

      val typeSymbols = tpe.decls.collect {
        case ts: TypeSymbol => (ts.name.toString, ts.toTypeIn(tpe))
      }.toMap

      val inFrom  = typeSymbols("InFrom")
      val outFrom = typeSymbols("InTo")

      val inner = findOrDeriveTransformer(inFrom, outFrom, cfg)

      val fn = TermName(c.freshName("fn"))

      q"$dependentTree.apply(($fn: $inFrom) => $inner.apply($fn))"
    }
  }

  private def findImplicit(tpe: Type): Option[Tree] =
    scala.util
      .Try(c.inferImplicitValue(tpe, silent = true, withMacrosDisabled = true))
      .toOption
      .filterNot(_ == EmptyTree)

  private def deriveTransformer(cfg: Config): c.Tree =
    if (isCaseClass(cfg.from) && isCaseClass(cfg.to)) {
      val fromParams = caseClassParams(cfg.from)
      val toParams   = caseClassParams(cfg.to)

      val from = TermName(c.freshName("from"))

      val args = toParams.map { ms =>
        val toName = ms.name.toString
        val outTpe = typeIn(ms, cfg.to)

        if (cfg.constFields.contains(toName))
          q"${cfg.ts}.applicative.pure[$outTpe](${cfg.consts}.apply($toName).asInstanceOf[$outTpe])"
        else {
          val fromName = cfg.mapping.getOrElse(toName, toName)
          val fromMs = fromParams
            .find(_.name.toString == fromName)
            .getOrElse(c.abort(c.enclosingPosition, s"Can't find source for $toName"))
          val fromTpe               = typeIn(fromMs, cfg.from)
          val underlyingTransformer = findOrDeriveTransformer(fromTpe, outTpe, cfg)
          q"""${cfg.ts}.errorSupport.addPath[$outTpe](
             $underlyingTransformer.apply($from.${fromMs.name}),
             _root_.transformers.common.ErrorPathNode.Accessor($fromName),
          )
          """
        }
      }

      q"""
         new _root_.transformers.common.Transformer[${cfg.f}, ${cfg.from}, ${cfg.to}] {
            def apply($from: ${cfg.from}): ${fTo(cfg)} =
              ${mkCaseClassF(cfg, args, toParams.map(typeIn(_, cfg.to)))}
         }
       """

    } else
      c.abort(c.enclosingPosition, s"Can't derive transformer from ${cfg.from} to ${cfg.to} in ${cfg.f}")

  private def mkCaseClassF(cfg: Config, args: List[Tree], targetTypes: List[Type]): Tree =
    if (args.size == 1) {
      q"${cfg.ts}.applicative.map[${targetTypes.head}, ${cfg.to}](${args.head})(new ${cfg.to}(_))"
    } else {
      val (productTree, productTpe) = args.tail.zip(targetTypes.tail).foldLeft((args.head, tq"${targetTypes.head}")) {
        case ((accTree, accTpe), (nextTree, nextTpe)) =>
          (q"${cfg.ts}.applicative.product[$accTpe, $nextTpe]($accTree, $nextTree)", tq"($accTpe, $nextTpe)")
      }
      val argTerms   = (1 to args.size).map(idx => c.internal.reificationSupport.freshTermName("arg$" + idx)).toList
      val emptyIdent = Ident(TermName("_"))
      val tuple = argTerms.tail
        .foldLeft[Tree](Bind(argTerms.head, emptyIdent))((acc, current) => q"($acc, ${Bind(current, emptyIdent)})")
      q"${cfg.ts}.applicative.map[$productTpe, ${cfg.to}]($productTree) { case $tuple => new ${cfg.to}(..$argTerms) }"
    }

  private def findOrDeriveTransformer(fromTpe: Type, outTpe: Type, cfg: Config): Tree = {
    val newCfg = Config(fromTpe, outTpe, cfg.f, cfg.ts)
    findImplicitTransformer(newCfg)
      .orElse(findImplicitDependentTransformer(newCfg))
      .getOrElse(deriveTransformer(newCfg))
  }

  private def typeIn(ms: MethodSymbol, caseClass: Type): Type =
    ms.typeSignatureIn(caseClass).finalResultType

  private def fTo(cfg: Config): Type = {
    val expanded = cfg.f.etaExpand
    expanded.finalResultType.substituteTypes(expanded.typeParams, List(cfg.to))
  }

  private def isCaseClass(tpe: Type): Boolean =
    tpe.typeSymbol.isClass && tpe.typeSymbol.asClass.isCaseClass

  private def caseClassParams(tpe: Type): List[MethodSymbol] =
    tpe.decls.collect { case m: MethodSymbol if m.isCaseAccessor => m }.toList

  @tailrec
  private def captureConfig(cfg: Type, prev: Config): Config =
    if (cfg =:= typeOf[TransformerConfig.Empty])
      prev
    else if (cfg.typeConstructor =:= typeOf[TransformerConfig.Const[_, _]].typeConstructor) {
      val List(const, next) = cfg.typeArgs
      captureConfig(next, prev.copy(constFields = prev.constFields + singletonString(const)))
    } else if (cfg.typeConstructor =:= typeOf[TransformerConfig.Mapping[_, _, _]].typeConstructor) {
      val List(from, to, next) = cfg.typeArgs
      captureConfig(next, prev.copy(mapping = prev.mapping.updated(singletonString(to), singletonString(from))))
    } else
      c.abort(c.enclosingPosition, "Invalid config")

  private def singletonString(tpe: Type): String =
    tpe
      .asInstanceOf[scala.reflect.internal.Types#UniqueConstantType]
      .value
      .value
      .toString
}
