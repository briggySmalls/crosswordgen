package mycrossword
package models

import mycrossword.models
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.util.matching.Regex

class CrosswordSpec extends AnyFlatSpec with should.Matchers with OptionValues {
  import CrosswordSpec._

  "Crossword.init" should "start with an initial word" in {
    val c = Crossword.init(brendfordWord)
    c should === (brentfordCrossword)
  }

  "Crossword.getUnblockedLetters" should "return whole initial word" in {
    brentfordCrossword.getUnblockedLetters(placedBrentfordWord) should ===(Crossword.string2Chars("brentford").zipWithIndex
      .map{ case (c, i) => Placed(
        Letter(c, Direction.Across), Placement(Index.zero.add(i, Direction.Across), Direction.Across)
      )}.toSet)
  }

  "Crossword.placeWord" should "place word correctly" in {
    val attemptWord = Word("attempt", "to try")
    val expected = toCrossword(
      Set(
      placedBrentfordWord,
      Placed(attemptWord, Placement(Index(-3, 2), Direction.Down))
    ),
      """
        |  |  |a↓|  |  |  |  |  |
        |  |  |t↓|  |  |  |  |  |
        |  |■■|t↓|■■|  |  |  |  |
        |b→|r→|e↓|n→|t→|f→|o→|r→|d→
        |  |■■|m↓|■■|  |  |  |  |
        |  |  |p↓|  |  |  |  |  |
        |  |  |t↓|  |  |  |  |  |
        |""".stripMargin,
      origin=Index(3, 0)
    )
    val result = brentfordCrossword.tryPlaceWord(
      attemptWord, Placed(Letter('e', Direction.Across), Placement(Index(0, 2), Direction.Across))
    )
    println(result.map(_.repr()))
    result.value should === (expected)
  }

  "Crossword.repr" should "render a simple crossword" in {
    brentfordCrossword.repr() should === ("b|r|e|n|t|f|o|r|d")
  }
}

object CrosswordSpec {
  val brendfordWord = Word("brentford", "by far the greatest team")
  val placedBrentfordWord = Placed(brendfordWord, Placement(Index.zero, Direction.Across))
  val brentfordCrossword = toCrossword(
    Set(Placed(brendfordWord, Placement(Index.zero, Direction.Across))),
    "b→|r→|e→|n→|t→|f→|o→|r→|d→".stripMargin
  )

  def toCrossword(words: Set[Placed[Word]], input: String, origin: Index = Index.zero): Crossword =
    Crossword(words, toGrid(input, origin))

  def toGrid(input: String, origin: Index): Grid = {
    val lines = input.split("\\n").filter(!_.isEmpty)
    val symbols = lines.map(_.split("\\|"))
    val positionedElements = for {
      (row, r) <- symbols.zipWithIndex
      (s, c) <- row.zipWithIndex
    } yield (Index(r, c) - origin, parseGridElementUnsafe(s))
    val positionedElementsWithoutEmpty = positionedElements.filter { case (_, el) => el != Empty}
    val sparseVector = positionedElementsWithoutEmpty.foldLeft(SparseVector.create[GridElement](Empty)) {
      case (sv, (i, el)) => sv.updated(i, el)
    }
    Grid(sparseVector)
  }

  private def parseGridElementUnsafe(symbol: String): GridElement = {
    val placedLetterRegex: Regex = """(\w)(→|↓)""".r
    symbol match {
      case "■■" => Blocked
      case "  "  => Empty
      case placedLetterRegex(letterStr, directionStr) =>
        val direction = if (directionStr == "→") Direction.Across else Direction.Down
        Filled(Letter(letterStr.charAt(0), direction))
      case _ => throw new RuntimeException(s"Unexpected symbol: \"$symbol\"")
    }
  }
}
