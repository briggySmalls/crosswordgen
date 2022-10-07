package mycrossword
package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class CrosswordSpec
    extends AnyFlatSpec
    with should.Matchers
    with ScalaCheckPropertyChecks {
  "A Crossword" should "product single option for first word" in {
    val word = Word("hello", "A greeting")
    Crossword(Set.empty, SparseVector.create(None), CandidateLookup.create())
      .placementChoices(word) should be(
      Set(Placed(word, Placement(Index(0, 0), Across)))
    )
  }

  it should "produce options for a word" in {
    val word = Word("hello", "A greeting")
    val placedWord = Word("echo", "A recurring sound")
    val charArray = placedWord.word.toCharArray
    val vectorMap = charArray.zipWithIndex
      .map { case (c, i) => Index(0, i) -> Some(Letter(c, Across)) }
      .toMap
      .withDefaultValue(None)
    val candidates = CandidateLookup.create(charArray.zipWithIndex.map {
      case (c, i) =>
        c -> Set(Index(0, i))
    }.toMap)

    Crossword(
      Set(Placed(placedWord, Placement(Index(0, 0), Across))),
      SparseVector(vectorMap),
      candidates
    ).placementChoices(word) should be(
      Set(
        Placed(word, Placement(Index(-1, 0), Down)),
        Placed(word, Placement(Index(0, 2), Down)),
        Placed(word, Placement(Index(-4, 3), Down))
      )
    )
  }

  it should "place the first word" in {}
}
