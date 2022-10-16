package mycrossword
package models

import models.Direction.{Across, Down}

case class Index(row: Int, column: Int) {
  def add(value: Int, direction: Direction) = direction match {
    case Direction.Down   => Index(row + value, column)
    case Direction.Across => Index(row, column + value)
  }

  def subtract(value: Int, direction: Direction) = add(value * -1, direction)

  def increment(direction: Direction) = add(1, direction)
}

object Index {
  val zero = Index(0, 0)
}
