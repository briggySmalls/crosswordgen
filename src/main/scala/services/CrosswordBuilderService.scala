package mycrossword
package services

import models.{Crossword, Letter, Placed, Word}

import mycrossword.services.CrosswordBuilderService.selectBestCrossword

import scala.annotation.tailrec
import scala.util.Random

class CrosswordBuilderService {
  def findGoodCrossword(words: Set[Word], iterations: Int): Crossword = {
    val trials = (1 to iterations).map(_ => buildCrossword(words))
    selectBestCrossword(trials, words.size)
  }

  def buildCrossword(words: Set[Word]): Crossword =
    // Sort words by length, starting with the longest
    words.toSeq.sortBy(_.word.length)(Ordering.Int.reverse) match {
      case Nil          => Crossword.empty // We weren't given any words to place!
      case head +: tail =>
        // Create a crossword with the first word
        val init = Crossword.init(head)
        // Try to place the rest
        placeWords(tail, init)
    }

  @tailrec
  private def placeWords(
      words: Seq[Word],
      crossword: Crossword
  ): Crossword =
    words match {
      case Nil          => crossword // All the candidates have been attempted!
      case head +: tail =>
        // Shuffle up the placed words to try match against
        val placedWords = Random.shuffle(crossword.words.toSeq)
        // Try to create a crossword by placing our next candidate
        val newCrossword = tryMatchWithPlacedWords(head, placedWords, crossword)
          .getOrElse(crossword)
        // Move on to the next candidate
        placeWords(tail, newCrossword)
    }

  @tailrec
  private def tryMatchWithPlacedWords(
      word: Word,
      targets: Seq[Placed[Word]],
      crossword: Crossword
  ): Option[Crossword] = targets match {
    case Nil =>
      None // All the placed words have been attempted for this candidate!
    case head +: tail =>
      // Shuffle up the valid letters to match our candidate word against
      val placedLetters =
        Random.shuffle(crossword.getUnblockedLetters(head).toSeq)
      // Try to match our candidate against the letters of a placed word
      tryMatchWithPlacedLetters(word, placedLetters, crossword) match {
        case c: Some[Crossword] => c // We succeeded! Short-circuit
        case None =>
          tryMatchWithPlacedWords( // Move on to the next placed word
            word,
            tail,
            crossword
          )
      }
  }

  @tailrec
  private def tryMatchWithPlacedLetters(
      word: Word,
      targets: Seq[Placed[Letter]],
      crossword: Crossword
  ): Option[Crossword] = targets match {
    case Nil          => None // We've tried to match against all the letters
    case head +: tail =>
      // Try to match our candidate against this placed letter
      crossword.tryPlaceWord(word, head) match {
        case c: Some[Crossword] => c // We succeeded! Short-circuit
        case None =>
          tryMatchWithPlacedLetters( // move on to the next placed letter
            word,
            tail,
            crossword
          )
      }
  }
}

object CrosswordBuilderService {
  def selectBestCrossword(
      cs: Seq[Crossword],
      candidateWordCount: Int
  ): Crossword =
    val maxArea = cs.foldLeft(0){case(area, c) => area.max(c.bounds.area)}
    val scoredCrosswords = cs.map(c =>
      (c, calculateScore(c, candidateWordCount, maxArea))
    )
    scoredCrosswords.reduce { case (best, candidate) =>
      if (candidate._2 > best._2) candidate
      else best
    }._1

  def calculateScore(
      c: Crossword,
      candidateWordCount: Int,
      maxSize: Int
  ): Double = {
    val numWords = c.words.size / candidateWordCount.toDouble
    val squareness = 1 - ((c.bounds.width - c.bounds.height) / (c.bounds.width + c.bounds.height).toDouble).abs
    val size = 1 - ((c.bounds.area) / maxSize.toDouble)
    Set(
      (1.0, numWords),
      (1.0, squareness),
      (1.0, size)
    ).foldLeft(0.0){ case (score, (w, s)) => score + w * s }
  }
}
