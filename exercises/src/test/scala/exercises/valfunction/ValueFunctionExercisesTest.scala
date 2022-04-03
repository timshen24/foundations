package exercises.valfunction

import exercises.valfunction.ValueFunctionExercises._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ValueFunctionExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  /////////////////////////////////////////////////////
  // Exercise 1: String API with higher-order functions
  /////////////////////////////////////////////////////

  // replace `ignore` by `test` to enable the test
  test("selectDigits examples") {
    assert(selectDigits("hello4world-80") == "480")
    assert(selectDigits("welcome") == "")
  }

  // replace `ignore` by `test` to enable the test
  test("selectDigits length is smaller") {
    forAll { (text: String) =>
      assert(selectDigits(text).length <= text.length)
    }
  }

  test("selectDigits must not have letter") {
    forAll { (text: String) =>
      selectDigits(text).foreach(c => assert(c.isDigit))
    }
  }

  test("secret must have same length and all asterisks") {
    forAll { (text: String) =>
      assert(secret(text).length == text.length && secret(text).forall(_ == '*'))
    }
  }

  test("secret is idempotent") {
    forAll { (text: String) =>
      val one = secret(text)
      val twice = secret(one)
      assert(one == twice)
    }
  }

  test("isValidUsername") {
    forAll {
      (username: String) =>
        assert(isValidUsername(username.reverse) == isValidUsername(username))
        assert(isValidUsername(username.toLowerCase) == isValidUsername(username.toLowerCase))
        assert(isValidUsername(username.toUpperCase) == isValidUsername(username.toUpperCase))
    }
  }
  ///////////////////////
  // Exercise 2: Point
  ///////////////////////
  test("isPositive of Point") {
    forAll { (x: Int, y: Int, z: Int) =>
      assert(Point(x, y, z).isPositive == List(x, y, z).forall(_ >= 0))
    }
  }

  test("Point isPositive") {
    forAll { (x: Int, y: Int, z: Int) =>
      assert(Point(x.max(0), y.max(0), z.max(0)).isPositive)
    }
  }

  test("Point isEven") {
    forAll { (x: Int, y: Int, z: Int) =>
      assert(if (Point(x, y, z).isEven)
        Point(x / 2 * 2, y / 2 * 2, z / 2 * 2) == Point(x, y, z)
      else true)
    }
  }

  test("Point forAll") {
    forAll { (x: Int, y: Int, z: Int, predicate: Int => Boolean) =>
      assert {
        Point(x, y, z).forAll(predicate) == List(x, y, z).forall(predicate)
      }
    }
  }
}
