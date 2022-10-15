package mycrossword
package models

case class Placement(point: Index, direction: Direction) {
  def add(value: Int): Placement =
    Placement(point.add(value, direction), direction)

  def subtract(value: Int): Placement = add(value * -1)

  def increment(): Placement = this.add(1)
}
