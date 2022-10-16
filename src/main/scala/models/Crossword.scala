package mycrossword
package models

import scala.annotation.tailrec
import scala.util.Try

case class Crossword(
    words: Set[Placed[Word]],
    grid: Grid
) {
  type SuccessfulPlacement = (Placement, Grid)

  def getUnblockedLetters(word: Placed[Word]): Set[Placed[Letter]] = {
    val letters = Crossword.toLetters(word)
    letters.filter(l => !grid.isBlocked(l.placement)).toSet
  }

  def tryPlaceWord(word: Word, target: Placed[Letter]): Option[Crossword] = {
    // See if any letters in the word match
    val letters = word.word.toCharArray.toSeq
    val matchingLettersWithIndex = letters.zipWithIndex.filter { case (c, i) =>
      c == target.item.char
    }
    // Determine candidate starting positions
    val direction = !target.placement.direction // Intersect perpendicularly
    val candidateStartingPlacements = matchingLettersWithIndex.map {
      case (_, i) =>
        // The start of the word will be offset from the intersection point
        val startingPoint = target.placement.point.subtract(i, direction)
        Placement(startingPoint, direction)
    }
    // See if we get a match
    val maybeNewGrid = tryPlaceWordByLetters(word.word, candidateStartingPlacements)
    maybeNewGrid.map { case (p, g) =>
      val placedWord = Placed(word, p)
      Crossword(words + placedWord, g)
    }
  }

  def repr(): String = {
    // Find the bounds
    val allBounds = words.map(_.bounds)
    val enclosingBounds = Bounds.enclosingBounds(allBounds)
    // Fetch the grid contents
    val elements = for {
      r <- enclosingBounds.min.row to enclosingBounds.max.row
      c <- enclosingBounds.min.column until enclosingBounds.max.column
    } yield {
      grid(Index(r, c))
    }
    // Build a string
    elements
      .grouped(enclosingBounds.width)
      .map(_.map(_.repr).mkString("|"))
      .mkString("\n")
  }

  @tailrec
  private def tryPlaceWordByLetters(
      word: String,
      startingPoints: Seq[Placement]
  ): Option[SuccessfulPlacement] = startingPoints match {
    case Nil          => None // We've run out of candidate starting points
    case head +: tail =>
      val maybeNewGrid = Crossword.tryPlaceLetters(word, head, grid)
      // If we find a new grid placement return it, otherwise try the next starting point
      maybeNewGrid match {
        case Some(g) => Some((head, g))
        case None => tryPlaceWordByLetters(word, tail)
      }
  }
}

object Crossword {
  def init(word: Word): Crossword = {
    val initialPlacement = Placement(Index(0, 0), Direction.Across)
    val grid = tryPlaceLetters(
      word.word,
      initialPlacement,
      Grid.create()
    )
    Crossword(
      Set(Placed(word, initialPlacement)),
      grid.get
    )
  }

  def toLetters(word: Placed[Word]): Seq[Placed[Letter]] = {
    word.item.word.toCharArray.zipWithIndex.map { case (c, i) =>
      Placed(Letter(c, word.placement.direction), word.placement.add(i))
    }
  }

  @tailrec
  private def tryPlaceLetters(
      letters: Seq[Char],
      placement: Placement,
      grid: Grid
  ): Option[Grid] = letters match {
    case Nil => Some(grid) // We've finished placing the letters!
    case head +: tail =>
      if (grid.fits(placement.point, head)) {
        val newGrid = grid.placeLetter(placement.point, Letter(head, placement.direction))
        tryPlaceLetters(tail, placement.increment(), newGrid)
      }
      else None
  }

  given string2Chars: Conversion[String, Seq[Char]] = _.toCharArray.toSeq

  val empty = Crossword(Set.empty, Grid.create())
}
