package Year2020


import org.scalacheck._
import org.scalatest.flatspec._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck._

import scala.io.Source

  class Day02Spec extends AnyFunSpec with Matchers with ScalaCheckPropertyChecks {

    import Year2020.Day02._

    describe("Part1") {
      val genPassword = for {
        letter <- Gen.alphaLowerChar
        min <- Gen.choose(1, 19)
        max <- Gen.choose(min, 20)
        value <- Gen.nonEmptyListOf(Gen.alphaLowerChar).map(_.mkString)
      } yield Password(letter, min, max, value)

      val genValidPassword = for {
        password <- genPassword
        chars <- Gen.choose(password.min, password.max).map(password.letter.toString * _)
        value = password.value.replace(password.letter.toString, "") + chars
      } yield Password(password.letter, password.min, password.max, value)

      val genInvalidPassword = for {
        password <- genPassword
        n <- Gen.choose(password.min, password.max)
        chars <- Gen.oneOf(Gen.choose(password.max + 1, password.max * 2).map(password.letter.toString * _), Gen.const(""))
        value = password.value.replace(password.letter.toString, "") + chars
      } yield Password(password.letter, password.min, password.max, value)

      val genPasswordList = Gen.nonEmptyListOf(Gen.oneOf(genValidPassword.map((_, true)), genInvalidPassword.map((_, false))))

      describe("countValidPasswords") {
        it("should return 2") {
          val passwords = List("1-3 a: abcde", "1-3 b: cdefg", "2-9 c: ccccccccc")
          val expected = 2
          val result = countValidPasswords(passwords, Part1)

          result shouldBe expected
        }

        it("should return expected number of valid passwords") {
          forAll(genPasswordList) { passwords =>
            val count = passwords.count(_._2)
            val ps = passwords.map(_._1.toString)
            val result = countValidPasswords(ps, Part1)

            result shouldBe count
          }
        }

        it("should return expected value from input list") {
          val filename = "./src/test/scala/Year2020/Day02-input.txt"
          val passwords = Source.fromFile(filename).getLines.toList
          val result = countValidPasswords(passwords, Part1)

          println(result)
        }
      }

      describe("Password.toString") {
        it("shouldbe the same as Password(Password.toString).toString") {
          forAll(genPassword) { password =>
            password.toString shouldBe Password(password.toString).toString
          }
        }
      }

      describe("Part1.isValid") {
        it("should return true when given a valid password") {
          forAll(genValidPassword) { password =>
            Part1.isValid(password) shouldBe true
          }
        }


        it("should return false when given an invalid password") {
          forAll(genInvalidPassword) { password =>
            Part1.isValid(password) shouldBe false
          }
        }
      }
    }

    describe("Part2") {
      def shiftChar(ch: Char, str: String): String = str.replace(ch, if (ch == 'z') 'a' else (ch.toInt+1).toChar)

      def insertChar(ch: Char, index: Int, str: String): String = {
        str.splitAt(index-1) match {
          case (s1, s2) => s"$s1$ch$s2"
        }
      }

      val genPassword = for {
        letter <- Gen.alphaLowerChar
        min <- Gen.choose(1, 19)
        max <- Gen.choose(min+1, 20)
        value <- Gen.listOfN(min + max, Gen.alphaLowerChar).map(_.mkString)
      } yield Password(letter, min, max, value)

      val genValidPassword = for {
        password <- genPassword
        n <- Gen.oneOf(password.min, password.max)
        value = insertChar(password.letter, n, shiftChar(password.letter, password.value))
      } yield Password(password.letter, password.min, password.max, value)

      val genInvalidPassword = for {
        password <- genPassword
        value = shiftChar(password.letter, password.value)
      } yield Password(password.letter, password.min, password.max, value)

      val genPasswordList = Gen.nonEmptyListOf(Gen.oneOf(genValidPassword.map((_, true)), genInvalidPassword.map((_, false))))


      describe("countValidPasswords") {
        it("return 1") {
          val passwords = List("1-3 a: abcde", "1-3 b: cdefg", "2-9 c: ccccccccc")
          val expected = 1
          val result = countValidPasswords(passwords, Part2)

          result shouldBe expected
        }

        it("should return expected number of valid passwords") {
          forAll(genPasswordList) { passwords =>
            val count = passwords.count(_._2)
            val ps = passwords.map(_._1.toString)
            val result = countValidPasswords(ps, Part2)

            result shouldBe count
          }
        }

        it("should return expected value from input list") {
          val filename = "./src/test/scala/Year2020/Day02-input.txt"
          val passwords = Source.fromFile(filename).getLines.toList
          val result = countValidPasswords(passwords, Part2)

          println(result)
        }

      }

      describe("Part2.isValid") {
        it("should return true when given a valid password") {
          forAll(genValidPassword) { password =>
            Part2.isValid(password) shouldBe true
          }
        }

        it("should return false when given an invalid password") {
          forAll(genInvalidPassword) { password =>
            Part2.isValid(password) shouldBe false
          }
        }
      }
    }
  }
