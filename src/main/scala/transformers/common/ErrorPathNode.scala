package transformers.common

sealed trait ErrorPathNode {
  def show: String

  def separator: String
}

object ErrorPathNode {
  final case class Accessor(name: String) extends ErrorPathNode {
    def show: String = name

    def separator: String = "."
  }

  final case class Index(value: Int) extends ErrorPathNode {
    def show: String = s"($value)"

    def separator: String = ""
  }

  final case class MapValue(key: AnyRef) extends ErrorPathNode {
    def show: String = s"($key)"

    def separator: String = ""
  }

  final case class MapKey(key: AnyRef) extends ErrorPathNode {
    def show: String = s"keys($key)"

    def separator: String = "."
  }

  def showErrorPath(list: List[ErrorPathNode]): String =
    list match {
      case head :: tail =>
        tail.foldLeft(head.show)((acc, next) => acc + next.separator + next.show)
      case Nil => ""
    }
}
