package mycrossword
package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatest.matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

class PlacementSpec
    extends AnyFlatSpec
    with matchers.should.Matchers
    with ScalaCheckPropertyChecks {
  import PlacementSpec._

  "Placement" should "add and subtract as opposites" in {
    forAll(indexGen, directionGen, arbitrary[Int]) {
      case (index, direction, inc) =>
        val start = Placement(index, direction)
        start.add(inc).subtract(inc) should ===(start)
    }
  }

  it should "do simple addition" in {
    Placement(Index.zero, Direction.Across).add(2) should ===(
      Placement(Index(0, 2), Direction.Across)
    )
  }

  "Placement.neighbours" should "provide all neighbours" in {
    Placement(Index.zero, Direction.Across)
      .neighbours() should ===(
      Set(
        Placement(Index(0, 1), Direction.Across),
        Placement(Index(0, -1), Direction.Across),
        Placement(Index(1, 0), Direction.Across),
        Placement(Index(-1, 0), Direction.Across)
      )
    )
  }

  "Placement.neighbours" should "provide just perp neighbours" in {
    Placement(Index.zero, Direction.Across)
      .neighbours(aligned = false) should ===(
      Set(
        Placement(Index(-1, 0), Direction.Across),
        Placement(Index(1, 0), Direction.Across)
      )
    )
  }
}

object PlacementSpec {
  val directionGen: Gen[Direction] = Gen.oneOf(Direction.Across, Direction.Down)
  val indexGen: Gen[Index] = for {
    row <- arbitrary[Int]
    col <- arbitrary[Int]
  } yield Index(row, col)
}
