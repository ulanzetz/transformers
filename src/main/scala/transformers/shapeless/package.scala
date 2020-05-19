package transformers

import transformers.common._
import _root_.shapeless.{::, HList, HNil, LabelledGeneric, Witness}
import _root_.shapeless.labelled.{field, FieldType}
import _root_.shapeless.ops._

package object shapeless {
  class TransformerBuilder[
    F[_]: Applicative,
    From,
    To,
    FromRepr <: HList,
    ToRepr <: HList,
    Mapping <: HList,
    ConstKeys <: HList
  ](from: From,
    lgenFrom: LabelledGeneric.Aux[From, FromRepr],
    lgenTo: LabelledGeneric.Aux[To, ToRepr],
    consts: Map[String, Any]
  ) {
    def mapping[FromField <: Symbol, ToField <: Symbol](
      from: Witness.Lt[FromField],
      to: Witness.Lt[ToField]
    )(
      implicit
      sel1: record.Selector[FromRepr, FromField],
      sel2: record.Selector[ToRepr, ToField]
    ): TransformerBuilder[F, From, To, FromRepr, ToRepr, FieldType[ToField, FromField] :: Mapping, ConstKeys] =
      this.asInstanceOf[TransformerBuilder[
        F,
        From,
        To,
        FromRepr,
        ToRepr,
        FieldType[ToField, FromField] :: Mapping,
        ConstKeys
      ]]

    def const[ToField <: Symbol, ToFieldValue](
      fieldName: Witness.Lt[ToField],
      value: ToFieldValue
    )(
      implicit
      w: Witness.Aux[ToField],
      sel: record.Selector.Aux[ToRepr, ToField, ToFieldValue]
    ): TransformerBuilder[F, From, To, FromRepr, ToRepr, Mapping, ToField :: ConstKeys] =
      new TransformerBuilder(from, lgenFrom, lgenTo, consts + (w.value.name -> value))

    def transform(implicit reprT: ReprTransformer[F, FromRepr, ToRepr, Mapping, ConstKeys]): F[To] =
      Applicative[F].map(reprT(lgenFrom.to(from), consts))(lgenTo.from)
  }

  trait ReprTransformer[F[_], FromRepr <: HList, ToRepr <: HList, Mapping <: HList, ConstKeys <: HList] {
    def apply(fromRepr: FromRepr, consts: Map[String, Any]): F[ToRepr]
  }

  object ReprTransformer extends LPReprTransformer {
    implicit def hnilT[
      F[_]: Applicative,
      FromRepr <: HList,
      Mapping <: HList,
      ConstKeys <: HList
    ]: ReprTransformer[F, FromRepr, HNil, Mapping, ConstKeys] =
      (_, _) => Applicative[F].pure(HNil)

    implicit def constHeadT[
      F[_]: Applicative,
      ToHeadKey <: Symbol,
      ToHeadValue,
      FromRepr <: HList,
      ToTail <: HList,
      Mapping <: HList,
      ConstKeys <: HList
    ](
      implicit
      constSel: hlist.Selector[ConstKeys, ToHeadKey],
      w: Witness.Aux[ToHeadKey],
      tailT: => ReprTransformer[F, FromRepr, ToTail, Mapping, ConstKeys]
    ): ReprTransformer[F, FromRepr, FieldType[ToHeadKey, ToHeadValue] :: ToTail, Mapping, ConstKeys] =
      (fromRepr, consts) =>
        Applicative[F].map(tailT(fromRepr, consts))(
          field[ToHeadKey](consts(w.value.name).asInstanceOf[ToHeadValue]) :: _
        )

    implicit def mappedHeadT[
      F[_],
      E,
      ToHeadKey <: Symbol,
      ToHeadValue,
      ToTail <: HList,
      FromRepr <: HList,
      FromKey <: Symbol,
      FromValue,
      Mapping <: HList,
      ConstKeys <: HList
    ](
      implicit
      ts: TransformationSupport[F, E],
      keySel: record.Selector.Aux[Mapping, ToHeadKey, FromKey],
      w: Witness.Aux[FromKey],
      valueSel: record.Selector.Aux[FromRepr, FromKey, FromValue],
      headT: Transformer[F, FromValue, ToHeadValue],
      tailT: => ReprTransformer[F, FromRepr, ToTail, Mapping, ConstKeys]
    ): ReprTransformer[F, FromRepr, FieldType[ToHeadKey, ToHeadValue] :: ToTail, Mapping, ConstKeys] =
      (from, consts) =>
        buildProduct[F, E, FromKey, ToHeadKey, ToHeadValue, ToTail](headT(valueSel(from)), tailT(from, consts))
  }

  trait LPReprTransformer {
    implicit def directHeadT[
      F[_],
      E,
      ToHeadKey <: Symbol,
      ToHeadValue,
      ToTail <: HList,
      FromRepr <: HList,
      FromValue,
      Mapping <: HList,
      ConstKeys <: HList
    ](
      implicit
      w: Witness.Aux[ToHeadKey],
      ts: TransformationSupport[F, E],
      valueSel: record.Selector.Aux[FromRepr, ToHeadKey, FromValue],
      headT: Transformer[F, FromValue, ToHeadValue],
      tailT: => ReprTransformer[F, FromRepr, ToTail, Mapping, ConstKeys]
    ): ReprTransformer[F, FromRepr, FieldType[ToHeadKey, ToHeadValue] :: ToTail, Mapping, ConstKeys] =
      (from, consts) =>
        buildProduct[F, E, ToHeadKey, ToHeadKey, ToHeadValue, ToTail](headT(valueSel(from)), tailT(from, consts))

    protected def buildProduct[F[_], E, FromKey <: Symbol, ToHeadKey <: Symbol, ToHeadValue, ToTail <: HList](
      headF: F[ToHeadValue],
      tailF: F[ToTail]
    )(
      implicit
      w: Witness.Aux[FromKey],
      ts: TransformationSupport[F, E]
    ): F[FieldType[ToHeadKey, ToHeadValue] :: ToTail] =
      ts.applicative.map(
        ts.applicative
          .product(ts.errorSupport.addPath(headF, ErrorPathNode.Accessor(w.value.name)), tailF)
      ) {
        case (head, tail) => field[ToHeadKey](head) :: tail
      }
  }

  implicit class TransformerBuilderSyntax[From](from: From) {
    def into[F[_]: Applicative, To]: TransformerBuilderHelper[F, From, To] =
      new TransformerBuilderHelper[F, From, To](from)
  }

  class TransformerBuilderHelper[F[_]: Applicative, From, To](from: From) {
    def using[FromRepr <: HList, ToRepr <: HList](
      implicit
      lgenFrom: LabelledGeneric.Aux[From, FromRepr],
      lgenTo: LabelledGeneric.Aux[To, ToRepr]
    ): TransformerBuilder[F, From, To, FromRepr, ToRepr, HNil, HNil] =
      new TransformerBuilder(from, lgenFrom, lgenTo, Map.empty)
  }

  implicit def defaultTransformer[F[_]: Applicative, From, To, FromRepr <: HList, ToRepr <: HList](
    implicit
    lgenFrom: LabelledGeneric.Aux[From, FromRepr],
    lgenTo: LabelledGeneric.Aux[To, ToRepr],
    reprT: ReprTransformer[F, FromRepr, ToRepr, HNil, HNil]
  ): Transformer[F, From, To] =
    from => Applicative[F].map(reprT(lgenFrom.to(from), Map.empty))(lgenTo.from)
}
