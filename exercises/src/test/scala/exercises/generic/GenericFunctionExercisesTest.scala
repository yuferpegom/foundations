package exercises.generic

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import exercises.generic.GenericFunctionExercises._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.util.Try

class GenericFunctionExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  ////////////////////
  // Exercise 1: Pair
  ////////////////////

  test("Pair swap") {
    forAll { (first: String, second: String) => 
      assert(Pair(first, second).swap == Pair(second, first))
    }
  }

  test("Pair map") {
    forAll { (first: Int, second: Int) =>
      assert(Pair(first, second).map(_+2) == Pair(first + 2, second + 2))
    }
    assert(Pair(1, 2).map(identity) == Pair(1, 2)) // we don't need more test if map is according to the rules
    // In the case o lists we ran more test because in theory it might behave different for short and long lists
  }

  test("Pair decoded") {
    assert(decoded == Pair("Functional", "Programming"))
  }

  test("Pair zipWith") { // you could use only one test!!!
    forAll { (first: Int, second: Int, f: (Int, Int) => Int) =>
      assert(Pair(first, second).zipWith(Pair(first, second))(f) == Pair(f(first, first), f(second, second)))

    }
  }

  test("Pair map3") { 
     Pair(0, 2).map3(Pair(3, 4), Pair(4, 5))((x, y, z) => x + y + z) == Pair(7, 10)
  }

  test("Pair productNames") {
    assert(products == Pair(Product("Coffee", 2.5), Product("Plane ticket", 329.99)))
  }

  ////////////////////////////
  // Exercise 2: Predicate
  ////////////////////////////

  import Predicate.{False, True}

  test("Predicate &&") {
    // Sometimes it is difficult to write a test that don't duplicate the implementation PBT
    assert((isEven && isPositive)(12) == true)
    assert((isEven && isPositive)(11) == false)
    assert((isEven && isPositive)(-4) == false)
    assert((isEven && isPositive)(-7) == false)
  }

  test("Predicate && PBT") {
    
    forAll { (eval1: Int => Boolean, eval2: Int => Boolean, value: Int) =>
      val p1 = Predicate(eval1)
      val p2 = Predicate(eval2)
      // Sometimes it is difficult to write a test that don't duplicate the implementation PBT
      // (p1 && p2)(value) == p1(value) && p2(value)
      // We can try using constants
      assert((p1 && False)(value) == false)
      assert((False && p2)(value) == false)
      assert((p1 && True)(value) == p1(value))  // This is known as a No-op
    }
  }

  test("Predicate ||") {
    assert((isEven || isPositive)(12) == true)
    assert((isEven || isPositive)(11) == true)
    assert((isEven || isPositive)(-4) == true)
    assert((isEven || isPositive)(-7) == false)
  }

  test("Predicate || PBT") { // help us to discover things about our API, eg. False and True might be useful
    
    forAll { (eval1: Int => Boolean, eval2: Int => Boolean, value: Int) =>
      val p1 = Predicate(eval1)
      val p2 = Predicate(eval2)
      // Sometimes it is difficult to write a test that don't duplicate the implementation PBT
      // (p1 && p2)(value) == p1(value) && p2(value)
      // We can try using constants
      assert((p1 || False)(value) == p1(value))
      assert((p1 || True)(value) == true)  // This is known as a No-op
    }
  }

  test("Predicate flip") {
    assert(isEven.flip(11) == true)
  }

  test("Is valid user") {
    assert(isValidUser(User("John", 20)) == true)
    assert(isValidUser(User("John", 17)) == false) // user is not an adult
    assert(isValidUser(User("john", 20)) == false) // name is not capitalized
    assert(isValidUser(User("x"   , 23)) == false )// name is too small
  }

  ////////////////////////////
  // Exercise 3: JsonDecoder
  ////////////////////////////

  test("JsonDecoder UserId") {
    assert(userIdDecoder.decode(JsonString("1234")) == UserId(1234))
    assert(Try(userIdDecoder.decode(JsonString("hello"))).toOption == None)
  }

  test("JsonDecoder UserId PBT") {
    forAll{ (id: Int) =>
      assert(userIdDecoder.decode(JsonString(id.toString)) == UserId(id))
    }
  }

  test("JsonDecoder LocalDate") {
    assert(localDateDecoder.decode(JsonString("\"2020-03-26\"")) == LocalDate.of(2020,3,26))
    assert(Try(localDateDecoder.decode(JsonString("2020-03-26"))).isFailure)
    assert(Try(localDateDecoder.decode(JsonString("hello"))).isFailure)
  }

  val genLocalDate: Gen[LocalDate] =
    Gen
      .choose(LocalDate.MIN.toEpochDay, LocalDate.MAX.toEpochDay)
      .map(LocalDate.ofEpochDay)
  
  implicit val arbitraryLocalDate: Arbitrary[LocalDate] = Arbitrary(genLocalDate)

  test("JsonDecoder LocalDate PBT") {
    forAll { (date: LocalDate) =>
      val json = s"\"${DateTimeFormatter.ISO_LOCAL_DATE.format(date)}\""
      assert(localDateDecoder.decode(JsonString(json)) == date)
    }
  }

  test("JsonDecoder weirdLocalDateDecoder") {
    forAll { (date: LocalDate) =>
      val json = s"\"${DateTimeFormatter.ISO_LOCAL_DATE.format(date)}\""
      val jsonEpoch = date.toEpochDay.toString
      assert(weirdLocalDateDecoder.decode(JsonString(json)) == date)
      assert(weirdLocalDateDecoder.decode(JsonInt(jsonEpoch)) == date)
    }
  }

}
