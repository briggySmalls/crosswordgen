package mycrossword
package models

import models.Direction.Across

sealed trait Direction {
  import Direction._

  def unary_! : Direction = this match {
    case Across => Down
    case Down   => Across
  }

  def repr: String
}

object Direction {
  object Across extends Direction { val repr = "across" }
  object Down extends Direction { val repr = "down" }
}
