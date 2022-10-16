package mycrossword
package models

case class Placement(point: Index, direction: Direction) {
  def add(value: Int): Placement =
    Placement(point.add(value, direction), direction)

  def subtract(value: Int): Placement = add(value * -1)

  def increment(): Placement = this.add(1)

  def neighbours(
      aligned: Boolean = true,
      perpendicular: Boolean = true
  ): Set[Placement] =
    val perpPlacements = if (perpendicular) Set(
      Placement(point.subtract(1, !direction), direction),
      Placement(point.add(1, !direction), direction)
    ) else Set.empty
    val alignedPlacements = if (aligned) Set(
      Placement(point.subtract(1, direction), direction),
      Placement(point.add(1, direction), direction)
    ) else Set.empty
    perpPlacements ++ alignedPlacements
}
