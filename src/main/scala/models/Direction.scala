package mycrossword
package models

trait Direction {
  def unary_! : Direction = this match {
    case Across => Down
    case Down   => Across
  }
}

object Across extends Direction
object Down extends Direction
