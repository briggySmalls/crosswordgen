package mycrossword
package models

sealed trait GridElement {
  def fits(char: Char): Boolean
  def repr: Char
  def isFilled: Boolean = this match {
    case _: Filled => true
    case _         => false
  }
}

case object Blocked extends GridElement {
  def fits(char: Char): Boolean = false
  def repr: Char = GridElement.blockedChar
}
case object Empty extends GridElement {
  def fits(char: Char): Boolean = true
  def repr: Char = GridElement.EmptyChar
}
case class Filled(letter: Letter) extends GridElement {
  def fits(char: Char): Boolean = letter.char == char
  def repr: Char = letter.char.toUpper
}

object GridElement {
  val blockedChar = '■'
  val EmptyChar = ' '
}
