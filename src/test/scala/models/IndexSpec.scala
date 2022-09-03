package mycrossword
package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import models.Direction

import org.scalacheck.Arbitrary.arbitrary
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class IndexSpec
    extends AnyFlatSpec
    with should.Matchers
    with ScalaCheckPropertyChecks {
  "An Index" should "increment by direction" in {
    forAll(arbitrary[Int], arbitrary[Int]) { (r: Int, c: Int) =>
      Index(r, c).increment(Across) should be(Index(r, c + 1))
    }
  }

  it should "add" in {
    forAll(arbitrary[Int], arbitrary[Int], arbitrary[Int]) {
      (r: Int, c: Int, n: Int) =>
        Index(r, c).add(n, Across) should be(Index(r, c + n))
    }
  }

  it should "subtract" in {
    forAll(arbitrary[Int], arbitrary[Int], arbitrary[Int]) {
      (r: Int, c: Int, n: Int) =>
        Index(r, c).subtract(n, Across) should be(Index(r, c - n))
    }
  }
}
