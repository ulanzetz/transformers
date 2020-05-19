package transformers.shapeless

import utest._
import transformers.ValidatedSuite
import transformers.ValidatedSupport._
import transformers.shapeless._
import transformers.common.syntax._
import transformers.validated._

object ShapelessTest extends ValidatedSuite {
  val tests = Tests {
    "simple case classes" - {
      case class From(a: String, b: String)

      case class To(a: Int, b: String)

      From("1", "a").transformTo[F, To] ==> Validated.Valid(To(1, "a"))

      From("a", "b").transformTo[F, To].showErrors ==> Validated.Invalid(Set("Can't parse int from a on a"))
    }

    "option unwrap" - {
      case class From(a: Option[String], b: Option[String])

      case class To(a: Int, b: String)

      From(Some("1"), Some("a")).transformTo[F, To] ==> Validated.Valid(To(1, "a"))

      From(Some("a"), None).transformTo[F, To].showErrors ==> Validated.Invalid(
        Set("Can't parse int from a on a", "Required value. Got None on b")
      )
    }

    "mapping and const" - {
      case class From(a: Option[String], b: Int, f: String, k: String)

      case class To(a: String, c: Int, c2: Int, b: Int, kkk: Int, e: Int)

      def transform(from: From): F[To] =
        from
          .into[F, To]
          .using
          .mapping('f, 'e)
          .mapping('k, 'kkk)
          .const('c, 100)
          .const('c2, 200)
          .transform

      transform(From(Some("a"), 1, "3", "4")) ==> Validated.Valid(To("a", 100, 200, 1, 4, 3))

      transform(From(None, 1, "a", "b")).showErrors ==> Validated.Invalid(
        Set("Required value. Got None on a", "Can't parse int from b on k", "Can't parse int from a on f")
      )
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
