package Year2020

import org.scalacheck._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck._
import org.scalatest.OptionValues._

import scala.io.Source

class Day01Spec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {
  private val numbers = Gen.nonEmptyListOf(Gen.choose(9, 2020)).map(_.distinct)
  private val lessThan1010 = Gen.nonEmptyListOf(Gen.choose(9, 1019)).map(_.distinct)
  private val lessThan505 = Gen.nonEmptyListOf(Gen.choose(9, 505)).map(_.distinct)

  "reportRepair2" should "find two that add to 2020 then multiply" in {
    forAll(lessThan1010) { (xs: List[Int]) =>
      val x = 2020 - xs.head
      val expected: Int = x * xs.head
      val result = Day01.reportRepair2(x :: xs)
//      println(s"$result == $expected")
      result should matchPattern { case Some((expected, _, _)) => }
    }
  }

  "reportRepair2" should "give specific list should return Some(514579, 1721, 299)" in {
    val filename = "./src/test/scala/Year2020/Day01-input.txt"
    val xs = List(1721, 979, 366, 299, 675, 1456)
    val expected = Some((514579, 1721, 299))

    val result = Day01.reportRepair2(xs)

//    println(s"$result == $expected")

    result shouldBe expected
  }

  "reportRepair2" should "using provided file list find two that add to 2020 then multiply them" in {
    val filename = "./src/test/scala/Year2020/Day01-input.txt"
    val xs = Source.fromFile(filename)
      .getLines
      .map(_.toIntOption)
      .filter(_.isDefined)
      .map(_.get)
      .toList

    val result = Day01.reportRepair2(xs)

    result should matchPattern { case Some((270144, _, _)) => }
  }

  "reportRepair3" should "find three that add to 2020 then multiply" in {
    forAll(lessThan505, lessThan505, numbers) { (xs: List[Int], ys: List[Int], zs: List[Int]) =>
      whenever(xs.nonEmpty && ys.nonEmpty && zs.nonEmpty && xs.head > 0 && ys.head > 0) {
        val x = 2020 - xs.head - ys.head
        val expected: Option[Int] = Some(x * xs.head * ys.head)
        val result = Day01.reportRepair3(zs ++ (x :: xs ++ ys))
        println(s"$result == $expected")
        result should matchPattern { case Some((expected, _, _, _)) => }
      }
    }
  }

  "reportRepair3" should "give specific list should return Some(241861950, 979, 366, 675)" in {
        val xs = List(1721, 979, 366, 299, 675, 1456)
        val expected = Some((241861950, 979, 366, 675))
        val result = Day01.reportRepair3(xs)
        println(s"$result == $expected")
        result shouldBe expected
  }

  "reportRepair3" should "using provided file list find two that add to 2020 then multiply them" in {
    val filename = "./src/test/scala/Year2020/Day01-input.txt"
    val xs = Source.fromFile(filename)
      .getLines
      .map(_.toIntOption)
      .filter(_.isDefined)
      .map(_.get)
      .toList

    val result = Day01.reportRepair3(xs)

    println(result)

//    result should matchPattern { case Some((270144, _, _)) => }
  }

}
