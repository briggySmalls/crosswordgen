package mycrossword
package models

sealed trait Placeable {
  def length: Int
}

case class Letter(char: Char, direction: Direction) extends Placeable {
  def length: Int = 0
}

case class Word(word: String) extends Placeable {
  def length: Int = word.length
}
