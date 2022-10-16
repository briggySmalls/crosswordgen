package mycrossword
package models

case class Bounds(min: Index, max: Index) {
  def width: Int = max.column - min.column
  def height: Int = max.row - min.row
}

object Bounds {
  def enclosingBounds(bounds: Set[Bounds]): Bounds = {
    val (top, bottom, right, left) = bounds.foldLeft((0, 0, 0, 0)) {
      case ((top, bottom, right, left), curr) =>
        val newTop = top.min(curr.min.row)
        val newBottom = bottom.max(curr.max.row)
        val newLeft = left.min(curr.min.column)
        val newRight = right.max(curr.max.column)
        (newTop, newBottom, newRight, newLeft)
    }
    Bounds(Index(top, left), Index(bottom, right))
  }
}
