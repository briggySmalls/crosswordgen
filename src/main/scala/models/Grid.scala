package mycrossword
package models

case class Grid(data: SparseVector[GridElement]) {
  def apply(index: Index): GridElement = data(index)

  def fits(index: Index, char: Char) = data(index).fits(char)

  def placeLetter(index: Index, letter: Letter): Grid = {
    // Add the letter
    val dataWithLetter = data.updated(index, Filled(letter))
    // Update blocked
    val wordDirection = letter.direction
    val perpDirection = !wordDirection
    val placements = Seq(
      index.add(1, perpDirection),
      index.subtract(1, perpDirection)
    ).map(Placement(_, wordDirection))
    val newData = placements.foldLeft(dataWithLetter) {
      case (data, placement) =>
        Grid.setBlockedStatus(data, placement)
    }
    Grid(newData)
  }

  def isBlocked(placement: Placement): Boolean = {
    val perpDirection = !placement.direction
    val index = placement.point
    val indicesToCheck = Seq(
      index.add(1, perpDirection),
      index.subtract(1, perpDirection)
    )
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
    val queryIndex =
      placement.point // The index we're determining the status for
    val indicesToCheck = Seq( // The indices we need to check
      placement.add(1).point,
      placement.subtract(1).point
    )
    indicesToCheck.foldLeft(data) { case (data, indexToCheck) =>
      data(indexToCheck) match {
        case Empty => data
        case _     => data.updated(queryIndex, Blocked)
      }
    }
  }
}
