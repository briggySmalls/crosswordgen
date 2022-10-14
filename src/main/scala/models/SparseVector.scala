package mycrossword
package models

case class SparseVector[T] (storage: Map[Index, T]) {
  def apply(index: Index) = storage(index)

  def updated(index: Index, value: T): SparseVector[T] =
    val newMap = storage + (index -> value)
    SparseVector(newMap)
}

object SparseVector {
  def create[T](default: T) =
    new SparseVector[T](Map[Index, T]().withDefaultValue(default))
}