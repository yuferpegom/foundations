package exercises.dataprocessing

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import exercises.dataprocessing.JsonExercises._

class JsonExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {
  val john: Json = JsonObject(
    Map(
      "name" -> JsonString(" John Doe "),
      "age"  -> JsonNumber(25),
      "address" -> JsonObject(
        Map(
          "street-number" -> JsonNumber(25),
          "street-name"   -> JsonString("  Cody Road"),
        )
      ),
    )
  )

  test("trimAll") {
    assert(
      trimAll(john) == JsonObject(
        Map(
          "name" -> JsonString("John Doe"),
          "age"  -> JsonNumber(25),
          "address" -> JsonObject(
            Map(
              "street-number" -> JsonNumber(25),
              "street-name"   -> JsonString("Cody Road"),
            )
          ),
        )
      )
    )
  }

  test("anonymize") {
    assert(
      anonymize(john) == JsonObject(
        Map(
          "name" -> JsonString("***"),
          "age"  -> JsonNumber(0),
          "address" -> JsonObject(
            Map(
              "street-number" -> JsonNumber(0),
              "street-name"   -> JsonString("***"),
            )
          ),
        )
      )
    )
  }

  test("search") {
    assert(search(JsonObject(Map.empty), "ll", 0) == false)
    assert(search(JsonNumber(5), "ll", 0) == false)
    assert(search(JsonString("Hello"), "ll", 0) == true)
    assert(search(JsonObject(Map("message" -> JsonString("Hello"))), "ll", 1) == true)
    assert(search(JsonObject(Map("message" -> JsonString("Hello"))), "ss", 1) == false)
    assert(search(JsonObject(Map("message" -> JsonString("hi"))), "ll", 1) == false)
    assert(search(john, "o", 2) == true)
    assert(search(john, "Cody", 1) == false)
  }

  test("depth") {
    assert(depth(JsonNumber(1)) == 0)
    assert(depth(JsonObject(Map.empty)) == 0)
    assert(depth(JsonObject(Map("k" -> JsonNumber(1)))) == 1)
    assert(depth(john) == 2)
  }

}
