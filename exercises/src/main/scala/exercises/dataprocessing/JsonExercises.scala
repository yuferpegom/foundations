package exercises.dataprocessing

object JsonExercises {

  sealed trait Json
  case class JsonNumber(number: Double)         extends Json
  case class JsonString(text: String)           extends Json
  case class JsonObject(obj: Map[String, Json]) extends Json
  case class JsonBoolean(obj: Boolean) extends Json
  case object JsonNull extends Json
  case class JsonArray(obj: Array[Json]) extends Json

  def trimAll(json: Json): Json =
    json match {
      case _: JsonNumber | _: JsonBoolean => json
      case JsonString(str) => JsonString(str.trim)
      case JsonArray(obj) => 
        val newObj = obj.map(trimAll)
        JsonArray(newObj)
      case JsonObject(obj) =>
        val newObj = obj.map {
          case (key, value) => (key, trimAll(value))
        }
        JsonObject(newObj)
        case JsonNull => JsonNull
    }

  // a. Implement `anonymize`, a method which keeps the structure of the JSON document
  // but removes all data such as:
  // * all `JsonString` are replaced by `***`.
  // * all `JsonNumbers` are replaced by 0
  // For example:
  // {                                          {
  //  "name": "John Doe",                         "name": "***",
  //  "age": 25,                                  "age": 0,
  //  "address": {                                "address": {
  //    "street": {             anonymize           "street": {
  //      "number" : 12,           ==>                "number" : 0,
  //      "name" : "Cody road"                        "name" : "***"
  //    },                                          },
  //    "country": "UK",                            "country": "***",
  //  }                                           }
  //}                                           }
  def anonymize(json: Json): Json =
    json match {
      case _: JsonNumber   => JsonNumber(0)
      case JsonString(_) => JsonString("***")
      case JsonBoolean(_) => JsonBoolean(false)
      case JsonArray(obj) =>
        val newObj = obj.map(anonymize)
        JsonArray(newObj)
      case JsonObject(obj) =>
        val newObj = obj.map {
          case (key, value) => (key, anonymize(value))
        }
        JsonObject(newObj)
      case _ => JsonNull
    }

  // b. Implement `search`, a method that checks if a JSON document contains a text.
  // Note: `search` doesn't look inside of the keys of a `JsonObject`, only the values.
  // For example:
  // * search({ }, "ll") == false
  // * search(5, "ll") == false
  // * search("Hello", "ll") == true
  // * search({ "message" : "hello" }, "ll") == true
  // * search({ "message" : "hi" }, "ll") == false
  def search(json: Json, text: String, d: Int): Boolean = {
        if(d < 0) false
        else {
        json match {
          case _: JsonNumber | _: JsonBoolean | JsonNull  => false
          case JsonString(jsonText) => jsonText.contains(text)
          case JsonArray(obj) =>
              obj.exists {
                value => search(value, text, d - 1)
              }
          case JsonObject(obj) =>
              obj.values.exists {
                value => search(value, text, d - 1)
              }
          }
        }
  }

  // c. Implement `depth`, a method that calculates the maximum level of nesting of a JSON document.
  // For example:
  // * { }, 5 or "hello" have depth 0
  // * { "name" : "john" } has depth 1
  // * { "name" : "john", "address" : { "postcode" : "E16 4SR" } } has depth 2
  def depth(json: Json): Int =
    json match {
      case _: JsonNumber | _: JsonString | _: JsonBoolean | JsonNull => 0 
      case JsonArray(obj) =>
        if(obj.isEmpty) 0
        else {
          1 + obj.map(depth).sum
        }
      case JsonObject(obj) =>
        if(obj.isEmpty) 0
        else {
          1 + obj.values.map(depth).sum
        }
    }

  //////////////////////////////////////////////
  // Bonus question (not covered by the video)
  //////////////////////////////////////////////

  // d. Add the missing cases to JSON enumeration: JsonBoolean, JsonArray and JsonNull.

  // e. add an extra parameter to search so that it limits the depth of the search.
  // such as search({ "user" : { "name" : "John" } }, "o", 2) == true
  // but     search({ "user" : { "name" : "John" } }, "o", 1) == false because "John" is at depth 2

}
