package Year2020

import org.scalacheck._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck._

import scala.io.Source

class Day01Spec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {
  private val lessThan1010 = Gen.nonEmptyListOf(Gen.choose(1, 1019))

  "reportRepair" should "find two that add to 2020 then multiply" in {
    forAll(lessThan1010) { (xs: List[Int]) =>
      val x = 2020 - xs.head
      val expected: Option[Int] = Some(x * xs.head)
      val result = Day01.reportRepair(x :: xs)
      //    println(s"$result == $expected")
      result shouldBe expected
    }
  }

  "reportRepair" should "using provided file list find two that add to 2020 then multiply them" in {
    val filename = "./src/test/scala/Year2020/Day01-input.txt"
    val xs = Source.fromFile(filename)
      .getLines
      .map(_.toIntOption)
      .filter(_.isDefined)
      .map(_.get)
      .toList

    val result = Day01.reportRepair(xs)

    result shouldBe Some(270144)
  }
}
