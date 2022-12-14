package mycrossword
package models

case class Grid(data: SparseVector[GridElement]) {
  def apply(index: Index): GridElement = data(index)

  def fits(placement: Placement, char: Char): Boolean = data(
    placement.point
  ) match {
    case Filled(l) =>
      l.char == char // If we're intersecting, our letter must match
    case _ =>
      placement
        .neighbours(aligned = false)
        .forall(p =>
          !data(p.point).isFilled
        ) // Otherwise, we need to make sure we're not brushing past a word
  }

  def placeLetter(index: Index, letter: Letter): Grid = {
    // Add the letter
    val dataWithLetter =
      if (!data(index).isFilled) data.updated(index, Filled(letter)) else data
    val wordDirection = letter.direction
    val perpDirection = !wordDirection
    // We must check either side of the letter
    val perpNeighbours =
      Placement(index, wordDirection).neighbours(aligned = false)
    val newData = perpNeighbours.foldLeft(dataWithLetter) {
      case (data, placement) =>
        Grid.setBlockedStatus(data, placement)
    }
    Grid(newData)
  }

  def isBlocked(placement: Placement): Boolean = {
    val index = placement.point
    val indicesToCheck =
      placement.neighbours(aligned = false).map(_.point)
    val contents = indicesToCheck.map(data(_))
    val blockedOnBothSides = contents.forall {
      case Blocked => true
      case _       => false
    }
    val hasALetter = contents.exists {
      case Filled(_) => true
      case _         => false
    }
    blockedOnBothSides || hasALetter
  }
}

object Grid {
  def create(): Grid = Grid(SparseVector.create(Empty))

  def setBlockedStatus(
      data: SparseVector[GridElement],
      placement: Placement
  ): SparseVector[GridElement] = {
    val queryIndex = placement.point
    data(queryIndex) match {
      case Empty => // We want to identify empty squares that are now blocked
        val indicesToCheck = placement
          .neighbours(perpendicular = false)
          .map(_.point) // The indices we need to check
        indicesToCheck.foldLeft(data) { case (data, indexToCheck) =>
          data(indexToCheck) match {
            case _: Filled => data.updated(queryIndex, Blocked)
            case _         => data
          }
        }
      case _ => data
    }
  }
}
