package mycrossword
package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class CrosswordSpec
    extends AnyFlatSpec
    with should.Matchers
    with ScalaCheckPropertyChecks {
  "A Crossword" should "place the first word" in {
    val word = Word("hello", "A greeting")
    Crossword(Set.empty, SparseVector.create(None), Map.empty)
      .placementChoices(word) should be(
      Set(Placed(word, Placement(Coordinate(0, 0), Across)))
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
    val candidates = charArray.zipWithIndex.map { case (c, i) =>
      c -> Set(Index(0, i))
    }.toMap
    Crossword(
      Set(
        Placed(
          placedWord,
          Placement(Coordinate(0, 0), Across)
        )
      ),
      SparseVector(vectorMap),
      candidates
    ).placementChoices(word) should be(
      Set(
        Placed(word, Placement(Coordinate(0, -1), Down)),
        Placed(word, Placement(Coordinate(2, 0), Down)),
        Placed(word, Placement(Coordinate(3, -4), Down))
      )
    )
  }
}
