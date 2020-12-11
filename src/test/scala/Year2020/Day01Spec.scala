package Year2020

import org.scalacheck._

object Day01Spec extends Properties("ReportRepairs") {
  val lessThan1010 = Gen.nonEmptyListOf(Gen.choose(1, 1019))

  property("find two that add to 2020 then squared") = Prop.forAll(lessThan1010) { (xs: List[Int]) =>
    val x = 2020 - xs.head
    val expected: Option[Int] = Some(x * xs.head)
    val result = Day01.reportRepair(x :: xs)
//    println(s"$result == $expected")
    result == expected
  }
}
