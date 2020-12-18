package Year2020

import Year2020.Day02.Password
import org.scalacheck._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck._

import scala.io.Source

class Day02Spec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {
  private val genPassword = for {
    letter <- Gen.alphaLowerChar
    min <- Gen.choose(1, 20)
    max <- Gen.choose(min, 20)
    value <- Gen.nonEmptyListOf(Gen.alphaLowerChar).map(_.mkString)
  } yield Password(letter, min, max, value)

  private val genValidPassword = for {
    password <- genPassword
    chars <- Gen.choose(password.min, password.max).map(password.letter.toString * _)
    value = password.value.replace(password.letter.toString, "") + chars
  } yield Password(password.letter, password.min, password.max, value)

  private val genInvalidPassword = for {
    password <- genPassword
    n <- Gen.choose(password.min, password.max)
    chars <- Gen.oneOf(Gen.choose(password.max + 1, password.max * 2).map(password.letter.toString * _), Gen.const(""))
    value = password.value.replace(password.letter.toString, "") + chars
  } yield Password(password.letter, password.min, password.max, value)

  private val genPasswordList = Gen.nonEmptyListOf(Gen.oneOf(genValidPassword.map((_, true)), genInvalidPassword.map((_, false))))

  "countValidPasswords" should "return 2" in {
    val passwords = List("1-3 a: abcde", "1-3 b: cdefg", "2-9 c: ccccccccc")
    val expected = 2
    val result = Day02.countValidPasswords(passwords)

    result shouldBe expected
  }

  "Password.toString" should "be the same as Password(Password.toString).toString" in {
    forAll(genPassword) { password =>
      password.toString shouldBe Password(password.toString).toString
    }
  }

  "Password.isValid" should "return true" in {
    forAll(genValidPassword) { password =>
      password.isValid shouldBe true
    }
  }

  "Password.isValid" should "return false" in {
    forAll(genInvalidPassword) { password =>
      password.isValid shouldBe false
    }
  }

  "countValidPasswords" should "return expected number of valid passwords" in {
    forAll(genPasswordList) { passwords =>
      val count = passwords.count(_._2)
      val ps = passwords.map(_._1.toString)
      val result = Day02.countValidPasswords(ps)

      result shouldBe count
    }
  }

  "countValidPasswords" should "return expected value from input list" in {
    val filename = "./src/test/scala/Year2020/Day02-input.txt"
    val passwords = Source.fromFile(filename).getLines.toList
    val result = Day02.countValidPasswords(passwords)

    println(result)
  }
}
