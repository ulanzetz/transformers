package transformers

import transformers.ValidatedSupport._
import transformers.common.TransformationError
import transformers.common.syntax._
import transformers.validated.{NEL, Validated}
import utest._

object CommonTest extends ValidatedSuite {
  val tests = Tests {
    "identity" - {
      1.transformTo[F, Int] ==> Validated.Valid(1)
    }

    "explicit defined" - {
      "1".transformTo[F, Int] ==> Validated.Valid(1)

      "a".transformTo[F, Int] ==> Validated.Invalid(NEL(TransformationError("Can't parse int from a")))
    }

    "lists" - {
      List("1", "2").transformTo[F, List[Int]] ==> Validated.Valid(List(1, 2))

      List("a", "b").transformTo[F, List[Int]].showErrors ==> Validated.Invalid(
        Set("Can't parse int from a on (0)", "Can't parse int from b on (1)")
      )
    }

    "maps" - {
      Map("1" -> "2", "3" -> "4").transformTo[F, Map[Int, Int]] ==> Validated.Valid(Map(1 -> 2, 3 -> 4))

      Map("a" -> "b", "c" -> "d").transformTo[F, Map[Int, Int]].showErrors ==> Validated.Invalid(
        Set(
          "Can't parse int from a on keys(a)",
          "Can't parse int from b on (a)",
          "Can't parse int from c on keys(c)",
          "Can't parse int from d on (c)"
        )
      )
    }

    "option unwrapping" - {
      Option("1").transformTo[F, Int] ==> Validated.Valid(1)

      Option("b").transformTo[F, Int] ==> Validated.Invalid(NEL(TransformationError("Can't parse int from b")))

      Option.empty[String].transformTo[F, Int] ==> Validated.Invalid(
        NEL(TransformationError("Required value. Got None"))
      )
    }
  }
}
