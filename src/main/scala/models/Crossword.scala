package mycrossword
package models

import scala.annotation.tailrec
import scala.util.Try

case class Crossword(
    words: Set[Placed[Word]],
    grid: SparseVector[Option[Letter]],
    candidateLetters: CandidateLookup
) {

  def placementChoices(word: Word): Set[Placed[Word]] = {
    if (words.isEmpty)
      Set(Placed(word, Placement(Index(0, 0), Across)))
    else
      word.word.toCharArray.toSeq.zipWithIndex.flatMap { case (_, i) =>
        placeWordByLetter(word, i)
      }.toSet
  }

  def placeWord(word: Placed[Word]): Crossword = {
    val point = word.placement.point
    val direction = word.placement.direction
    val newWords = words + word
    val letters = word.item.word.toCharArray.toSeq
    val newGrid = letters.zipWithIndex.foldLeft(grid) { case (g, (c, i)) =>
      g.updated(point.add(i, direction), Some(Letter(c, direction)))
    }
    val newCandidates = letters.zipWithIndex.foldLeft(candidateLetters) {
      case (lookup, (char, inc)) =>
        val currentPoint = point.add(inc, direction)
        if (grid(currentPoint).isDefined) {
          // Remove this letter and either side
          (-1 to 1)
            .map(i => currentPoint.add(i, !direction))
            .foldLeft(lookup) { case (l, p) =>
              removeCandidate(p)
            } // TODO, move this to CandidateLetters
        } else {
          // Add letter
          lookup.append(char, currentPoint)
        }
    }
    // Remove candidates off the ends
    Seq(
      point.add(-1, direction),
      point.add(word.item.word.length, direction)
    ).foldLeft(candidateLetters) { case (cl, i) =>
      removeCandidate(i)
    } // TODO, move this to CandidateLetters
    Crossword(newWords, newGrid, newCandidates)
  }

  private def placeWordByLetter(word: Word, letter: Int): Set[Placed[Word]] =
    (for {
      char <- Try(word.word.charAt(letter)).toOption
      placedLetters = candidateLetters.get(char)
    } yield {
      placedLetters.flatMap(index => tryToPlace(word, letter, index))
    }).getOrElse(Set.empty)

  private def tryToPlace(
      word: Word,
      letter: Int,
      index: Index
  ): Option[Placed[Word]] =
    grid(index)
      .flatMap(l => {
        val direction = !(l.direction)
        val chars = word.word.toList
        val offset = letter
        val startingIndex = index.subtract(offset, direction)
        if (testIfFits(chars, startingIndex, direction))
          Some(
            Placed(
              word,
              Placement(
                startingIndex,
                direction
              )
            )
          )
        else None
      })

  @tailrec
  private def testIfFits(
      letters: Seq[Char],
      index: Index,
      direction: Direction
  ): Boolean =
    letters match {
      case Nil => true
      case head +: tail => {
        val isValid = grid(index).map(l => l.char == head).getOrElse(true)
        if (isValid) testIfFits(tail, index.increment(direction), direction)
        else false
      }
    }

  private def removeCandidate(point: Index): CandidateLookup = {
    grid(point) match {
      case Some(letter) =>
        grid(point)
          .map(l => candidateLetters.remove(l.char, point))
          .getOrElse(candidateLetters)
      case None => candidateLetters
    }
  }
}
