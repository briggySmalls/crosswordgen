package mycrossword
package models

import models.Direction.Across

sealed trait Direction {
  import Direction._

  def unary_! : Direction = this match {
    case Across => Down
    case Down   => Across
  }
}

object Direction {
  object Across extends Direction
  object Down extends Direction
}
