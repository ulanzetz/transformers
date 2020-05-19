package transformers.macros

import utest._
import transformers.common.syntax._
import transformers.ValidatedSuite
import transformers.ValidatedSupport._
import transformers.common.{TransformationError, Transformer}
import transformers.macros._
import transformers.validated.{NEL, Validated}

object MacrosTest extends ValidatedSuite {
  implicit def optExtract[A, B]: DependentTransformer.Aux[F, A, B, Option[A], B] =
    DependentTransformer.build(
      from =>
        underlying =>
          from.fold[F[B]](Validated.Invalid(NEL(TransformationError("Required value. Got None"))))(underlying(_))
    )

  val tests = Tests {
    "mapping and consts" - {
      case class From(a: String, b: String)

      case class To(c: Int, d: Int, e: Int)

      From("1", "2")
        .into[F, String, To]
        .mapping(_.a, _.c)
        .mapping(_.b, _.d)
        .const(_.e, 3)
        .transform ==>
      Validated.Valid(To(1, 2, 3))
    }

    "inner case classes" - {
      case class From(a: String, b: Option[InnerFrom])

      case class InnerFrom(c: String, d: Option[Int])

      case class To(a: Int, b: InnerTo)

      case class InnerTo(c: Int, d: Int)

      From("1", Some(InnerFrom("2", Some(3)))).transformTo[F, To] ==> Validated.Valid(To(1, InnerTo(2, 3)))

      From("a", Some(InnerFrom("b", None))).transformTo[F, To].showErrors ==> Validated.Invalid(
        Set("Can't parse int from a on a", "Can't parse int from b on b.c", "Required value. Got None on b.d")
      )
    }
  }
}
