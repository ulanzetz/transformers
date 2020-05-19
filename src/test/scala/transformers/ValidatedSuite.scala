package transformers

import transformers.ValidatedSupport.Result
import transformers.common.{ErrorPathNode, TransformationError, Transformer}
import transformers.validated.{NEL, Validated}
import utest.TestSuite

trait ValidatedSuite extends TestSuite {
  type F[A] = Result[String, A]

  implicit val intParse: Transformer[F, String, Int] =
    str =>
      str.toIntOption
        .fold[F[Int]](Validated.Invalid(NEL(TransformationError(s"Can't parse int from $str"))))(Validated.Valid(_))

  implicit def optionExtract[A, B](implicit underlying: Transformer[F, A, B]): Transformer[F, Option[A], B] =
    _.fold[F[B]](Validated.Invalid(NEL(TransformationError("Required value. Got None"))))(underlying(_))

  implicit class ShowErrorSyntax[A](fa: F[A]) {
    def showErrors: Validated[Set[String], A] =
      fa match {
        case Validated.Invalid(nel) =>
          Validated.Invalid((nel.head :: nel.tail).map { error =>
            val on = if (error.path.isEmpty) "" else s" on ${ErrorPathNode.showErrorPath(error.path)}"
            s"${error.message}$on"
          }.toSet)
        case Validated.Valid(valid) => Validated.Valid(valid)
      }
  }
}
