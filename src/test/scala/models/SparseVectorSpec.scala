package mycrossword
package models

import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class SparseVectorSpec
    extends AnyFlatSpec
    with should.Matchers
    with ScalaCheckPropertyChecks {

  val abitraryIndex: Gen[Index] = for {
    row <- arbitrary[Int]
    col <- arbitrary[Int]
  } yield Index(row, col)

  "A SparseVector" should "have a default value" in {
    forAll(abitraryIndex) { (index: Index) =>
      SparseVector.create(0)(index) should be(0)
    }
  }

  it should "store a value" in {
    forAll(abitraryIndex, arbitrary[Int]) { (index: Index, value: Int) =>
      SparseVector.create(0).updated(index, value)(index) should be(value)
    }
  }
}
