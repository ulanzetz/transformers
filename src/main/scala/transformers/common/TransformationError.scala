package transformers.common

case class TransformationError[M](message: M, path: List[ErrorPathNode] = Nil) {
  def prepend(node: ErrorPathNode): TransformationError[M] =
    TransformationError[M](message, node :: path)
}
