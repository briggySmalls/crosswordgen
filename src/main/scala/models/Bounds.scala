package mycrossword
package models

case class Bounds(min: Index, max: Index) {
  def width: Int = max.row - min.row
  def height: Int = max.column - min.column
}

object Bounds {
  def enclosingBounds(bounds: Set[Bounds]): Bounds = {
    val (top, bottom, right, left) = bounds.foldLeft((0, 0, 0, 0)) {
      case ((top, bottom, right, left), curr) =>
        val newTop = top.min(curr._2.column)
        val newBottom = bottom.max(curr._1.column)
        val newRight = right.max(curr._2.row)
        val newLeft = left.min(curr._1.row)
        (newTop, newBottom, newRight, newLeft)
    }
    Bounds(Index(left, top), Index(right, bottom))
  }
}
