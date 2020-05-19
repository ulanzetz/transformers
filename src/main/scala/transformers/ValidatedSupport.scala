package transformers

import transformers.common._
import transformers.validated._

object ValidatedSupport {
  type Result[M, A] = Validated[NEL[TransformationError[M]], A]

  implicit def applicativeInstance[M]: Applicative[Result[M, *]] = new Applicative[Result[M, *]] {
    override def pure[A](x: A): Result[M, A] =
      Validated.Valid(x)

    override def product[A, B](fa: Result[M, A], fb: Result[M, B]): Result[M, (A, B)] =
      (fa, fb) match {
        case (Validated.Valid(a), Validated.Valid(b))           => Validated.Valid((a, b))
        case (Validated.Invalid(nel1), Validated.Invalid(nel2)) => Validated.Invalid(NEL.combine(nel1, nel2))
        case (invalid @ Validated.Invalid(_), _)                => invalid
        case (_, invalid @ Validated.Invalid(_))                => invalid
      }

    override def map[A, B](fa: Result[M, A])(f: A => B): Result[M, B] =
      fa match {
        case Validated.Valid(a)     => Validated.Valid(f(a))
        case Validated.Invalid(nel) => Validated.Invalid(nel)
      }
  }

  implicit def errorSupportInstance[M]: ErrorSupport[Result[M, *], M] = new ErrorSupport[Result[M, *], M] {
    override def addPath[A](fa: Result[M, A], node: ErrorPathNode): Result[M, A] =
      fa match {
        case Validated.Invalid(nel) => Validated.Invalid(NEL.map(nel)(_.prepend(node)))
        case valid                  => valid
      }

    override def raiseError[A](e: M): Result[M, A] =
      Validated.Invalid(NEL(TransformationError(e)))
  }

  implicit def transformationSupportInstance[M]: TransformationSupport[Result[M, *], M] =
    new TransformationSupport[Result[M, *], M] {
      override def applicative: Applicative[Result[M, *]] = applicativeInstance

      override def errorSupport: ErrorSupport[Result[M, *], M] = errorSupportInstance
    }
}
