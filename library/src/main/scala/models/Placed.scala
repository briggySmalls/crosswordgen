package mycrossword
package models

case class Placed[T <: Placeable](item: T, placement: Placement) {
  def bounds: Bounds =
    Bounds(placement.point, placement.add(item.length).point)
}
