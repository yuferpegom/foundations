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

  test("selectDigits all are digits") {
    forAll { (text: String) =>
      selectDigits(text).foreach(c => assert(c.isDigit))
    }
  }

  ///////////////////////
  // Exercise 2: Point
  ///////////////////////

  test("secret all are *") {
    forAll { (text: String) =>
      secret(text).foreach(_.equals('*'))
    }
  }

  test("is valid username") {
    forAll { (username: String) =>
      assert(isValidUsername(username.reverse) == isValidUsername(username))
      assert(isValidUsername(username.toLowerCase) == isValidUsername(username.toUpperCase))
    }
  }

  test("Is positive") {
    forAll { (x: Int, y: Int, z: Int) =>
      // assert(Point(x.abs, y.abs, z.abs).isPositive) // when abs is call in int.min it returns a negative (-2147483648)
      assert(Point(x.max(0), y.max(0), z.max(0)).isPositive)
    }
  }

  test("for all") {
    forAll { (x: Int, y: Int, z: Int, predicate: Int => Boolean) =>
      // assert(Point(x.abs, y.abs, z.abs).isPositive) // when abs is call in int.min it returns a negative (-2147483648)
      assert(Point(x.max(0), y.max(0), z.max(0)).forAll(_ >= 0))

      assert(Point(x, y, z).forAll(predicate) == List(x, y, z).forall(predicate))
    }
  }

}
