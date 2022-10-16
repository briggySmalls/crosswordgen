package mycrossword
package models

import models.Direction.{Across, Down}

case class Index(row: Int, column: Int) {
  def add(value: Int, direction: Direction) = direction match {
    case Direction.Down   => Index(row + value, column)
    case Direction.Across => Index(row, column + value)
  }

  def subtract(value: Int, direction: Direction) = add(value * -1, direction)

  def +(other: Index) = Index(row + other.row, column + other.column)
  def -(other: Index) = this + (other * -1)
  def *(value: Int) = Index(row * value, column * value)

  def increment(direction: Direction) = add(1, direction)
}

object Index {
  val zero = Index(0, 0)
}
