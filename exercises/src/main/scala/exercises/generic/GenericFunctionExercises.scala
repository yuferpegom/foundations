package exercises.generic

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import scala.util.{Failure, Success, Try}

// Notes

// missing parameter type for expanded the compiler cannnot use type info aquired from one arg to 
// infer the type of another arg in the same parenthsis
// def map[From, To](list: List[From], update: From => To) : List[To]
// So in scala 2 we need to use it like
// val users = List(User....)
// map(users, user: User => user.name) // note we need to annotate the type in the update function
// Solution:
// Use two parameter groups:
// def map[From, To](list: List[From])(update: From => To)
// map(users)(_.name)  this now works
// No need in Scala 3

// 1.  Gen fun should accept all types
// map[String, Int]
// map[User, Address]
// 2. All types must be treated the same way
// This should be avoided:
// def format[A](vale: A): String =
  // value match {
  // case x: String => x.toLowerCase
  // case x: Double => truncate(2, x)
  // case _ => "N/A"
  //}
// Why? Because of TYPE ERASURE
/**
 * If we add more cases like this
 * 
 * def format[A](vale: A): String =
  value match {
   case x: String => x.toLowerCase
   case x: Double => truncate(2, x)
   case x: List[String] => x.map(_.toLowerCase).mkString("")
   case x: List[Double] => x.map(truncate(2, _)).mkString("")
   case _ => "N/A"
  }
It will work for the first cases but for doubles will have an error:
  format(List(123.23123, 123123,024))
  // cannot be cast to class java.lang.String

  Pattern matching relies on instanceOf
  At runtime it cannot determine wheter it is a List[String] or a List[Double] because at runtime it will be a List[Any|Object]
  For scala 2.13 we cannot change the warning to make it an error

  ---PATTERN MATCHING ON A GENERIC VALUE not a good idea
  - Poor documentation
  def format[A](value: A): String
  - How to test format without looking at its implementation? 
  - Which type does it pattern matc on?

  --THE MORE GENERIC A FUNCTION IS THE MORE PRECISE IT IS:
    map[From, To](list: List[From])(udpate: From => To): List[To] => Ensures that if list is not empty, the list and the update must be used
    map[A, A](list: List[A])(update: A => A): List[A] => No need to use upate you could just return the input list
    map(list: List[Int])(update: Int => Int): List[Int] => You can avoud both the input and the update just by returning any random List(1,2,3)

  This improves testing, 
  1. if map works in ints and all types must be treated equally, it must work for all other types so no need
  to test over all types!
  2. No need to test different update functions as we can be sure it must be used so if it works for once it is enough
  ej: You could test using the identity function

  Summary:
    - Caller decices which type to use!
    - More reusable, less test, better docs


**/

object GenericFunctionExercises {

  ////////////////////
  // Exercise 1: Pair
  ////////////////////

  val names: Pair[String] = Pair("John", "Elisabeth")
  val ages: Pair[Int]     = Pair(32, 46)
  /** 
   * In the companion you can add methods that don't require an instance of the class like static methods or constants
   **/
  case class Pair[A](first: A, second: A) {
    // 1a. Implement `swap` which exchanges `first` and `second`
    // such as Pair("John", "Doe").swap == Pair("Doe", "John")
    def swap: Pair[A] =
      Pair(second, first)

    // 1b. Implement `map` which applies a function to `first` and `second`
    // such as Pair("John", "Doe").map(_.length) == Pair(4,3)
    def map[To](update: A => To): Pair[To] =
      Pair(update(first), update(second))

    // 1c. Implement `zipWith` which merges two Pairs using a `combine` function
    // such as Pair(0, 2).zipWith(Pair(3, 4))((x, y) => x + y) == Pair(3, 6)
    //         Pair(2, 3).zipWith(Pair("Hello ", "World "))(replicate) == Pair("Hello Hello ", "World World World ")
    // Bonus: Why did we separate the arguments of `zipWith` into two set of parentheses? To get better type inference
    // AVOID MISSING PARAM TYPE FOR EXPANDED FUNCTION
    def zipWith[Other, To](other: Pair[Other])(combine: (A, Other) => To): Pair[To] =
      Pair(combine(this.first, other.first), combine(this.second, other.second)) 

    def map3[Another, Other, To](other: Pair[Other], another: Pair[Another])(combine: (A, Other, Another) => To): Pair[To] = {

      val combine1 = (a: A, o: Other) => (an: Another) => combine(a, o, an)
      val p: Pair[Another => To] = this.zipWith(other)(combine1)
      val combine2 = (f: Another => To, input: Another) => f(input)
      p.zipWith(another)(combine2)
    }

  }

  // 1d. Use the Pair API to decode the content of `secret`.
  // Note: You can transform a Byte into a Char using `byte.toChar`
  //       or you can create a String from an Array[Byte] using `new String(byteArray)`
  // Note: You can remove the lazy keyword from `decoded` once you have implemented it.
  val secret: Pair[List[Byte]] =
    Pair(
      first = List(103, 110, 105, 109, 109, 97, 114, 103, 111, 114, 80),
      second = List(108, 97, 110, 111, 105, 116, 99, 110, 117, 70)
    )
  val decoded: Pair[String] =
    secret.map(_.map(_.toChar).reverse.mkString).swap 

  // 1e. Use the Pair API to combine `productNames` and `productPrices` into `products`
  // such as products == Pair(Product("Coffee", 2.5), Product("Plane ticket", 329.99))
  case class Product(name: String, price: Double)

  val productNames: Pair[String]  = Pair("Coffee", "Plane ticket")
  val productPrices: Pair[Double] = Pair(2.5, 329.99)

  lazy val products: Pair[Product] =
    productNames.zipWith(productPrices)(Product) 

  //////////////////////////////////////////////
  // Bonus question (not covered by the video)
  //////////////////////////////////////////////

  // 1f. Can you implement a method on `Pair` similar to `zipWith`, but that combines 3
  // Pairs instead of 2? If yes, can you implement this method using `zipWith`?
  // Note: Libraries often call this method `map3` and `zipWith` is often called `map2`

  ////////////////////////////
  // Exercise 2: Predicate
  ////////////////////////////

  val isPositive: Predicate[Int] =
    Predicate((number: Int) => number >= 0)

  val isEven: Predicate[Int] =
    Predicate((number: Int) => number % 2 == 0)

  lazy val isOddPositive: Predicate[Int] =
    isEven.flip && isPositive

  case class Predicate[A](eval: A => Boolean) {
    // DSL to call a predicate like a function
    // isPositive(10) instead of isPositive.eval(10)
    def apply(value: A): Boolean = eval(value)

    // 2a. Implement `&&` that combines two predicates using logical and
    // such as (isEven && isPositive)(12) == true
    // but     (isEven && isPositive)(11) == false
    //         (isEven && isPositive)(-4) == false
    //         (isEven && isPositive)(-7) == false
    def &&(other: Predicate[A]): Predicate[A] = 
      Predicate( (a: A) => this.eval(a) && other.eval(a))

    // 2b. Implement `||` that combines two predicates using logical or
    // such as (isEven || isPositive)(12) == true
    //         (isEven || isPositive)(11) == true
    //         (isEven || isPositive)(-4) == true
    // but     (isEven || isPositive)(-7) == false
    def ||(other: Predicate[A]): Predicate[A] =
      Predicate( (a: A) => this.eval(a) || other.eval(a))

    // 2c. Implement `flip` that reverses a predicate
    // such as isEven.flip(11) == true
    def flip: Predicate[A] =
      Predicate( (a: A) => !this.eval(a))

    def contramap[To](zoom: To => A): Predicate[To] = 
      Predicate(user => eval(zoom(user))) 

  }

  object Predicate { 
    //True an false ARE predicates so seem to fit better in the companion object
      def False[A] = Predicate[A](_ => false)
      def True[A] = False[A].flip
  }

  // 2d. Implement `isValidUser`, a predicate which checks if a `User` is:
  // * an adult (older than 18 year) and
  // * their name is longer than or equal to 3 characters and
  // * their name is capitalized, meaning that it starts with an uppercase letter
  // such as isValidUser(User("John", 20)) == true
  // but     isValidUser(User("John", 17)) == false // user is not an adult
  //         isValidUser(User("john", 20)) == false // name is not capitalized
  //         isValidUser(User("x"   , 23)) == false // name is too small
  // Note: Can you use methods from the Predicate API such as `&&`, `||` or `flip`?
  // You may want to create new Predicate methods to improve the implementation of `isValidUser`.
  case class User(name: String, age: Int)

  // lazy val isValidUser: Predicate[User] = 
    // Predicate { user =>
    //   user.age >= 18 &&
    //   user.name.length >= 3 &&
    //   user.name.capitalize == user.name
    // } // good but we'd like to use the predicate api
  //  (isAdult && correctName && nameIsCapitalized) // This doesn't show the hierarchical organization of predicates. we have 3 sub predicates
   // that doesn't tell us anything about how they are organized (eg we don't now the 1st one targets the age and the others the name)
   // Can we group them by field???

  // val isAdult: Predicate[User] = Predicate(_.age >= 18)
  // val correctName: Predicate[User] = Predicate(_.name.length >= 3)
  // val nameIsCapitalized: Predicate[User] = Predicate(user => user.name == user.name.capitalize) // cannot user the _ notation

  // Improving our Predicate API
  def isBiggerThan(min: Int): Predicate[Int] = 
    Predicate[Int](_ >= min)

  def isLongerThan(min: Int): Predicate[String] =
    isBiggerThan(min).contramap(_.length)

  val isCapitalized: Predicate[String] =
    Predicate(text => text.capitalize == text)

  val isAdult: Predicate[User] = isBiggerThan(18).contramap(_.age)
  val correctName: Predicate[User] = isBiggerThan(3).contramap(_.name.length)
  val nameIsCapitalized: Predicate[User] = Predicate(user => user.name == user.name.capitalize) // cannot user the _ notation


  //GROUPED By FIEDL fun
  lazy val isValidUser: Predicate[User] = {
    by[User, Int](_.age)(isBiggerThan(18)) &&
    by[User, String](_.name)(isLongerThan(3) && isCapitalized)
  }
    // by(_.name)(??? : Predicate[String])

  def by[From, To](value: From => To)(predicate: Predicate[To]): Predicate[From] = 
    predicate.contramap(value)
  ////////////////////////////
  // Exercise 3: JsonDecoder
  ////////////////////////////

  // very basic representation of JSON
  // type Json = String

  trait Json {
    val value: String
  }
  sealed case class JsonInt(value: String) extends Json
  sealed case class JsonString(value: String) extends Json

  trait JsonDecoder[A] { outer => //This is a way to get the self-reference
    def decode(json: Json): A // Sam Single Abstract method


    def map[To](update: A => To): JsonDecoder[To] = new JsonDecoder[To] {
      def decode(json: Json): To =
        update(outer.decode(json))     // outer is the self-type or self-reference
    }
    /* [error]  found   : To
      [error]  required: A
      [error]         update(this.decode(json)) 
      */
    // def map1[To](update: A => To): JsonDecoder[To] = new JsonDecoder[To] {
    //   def decode(json: Json): To =
    //     update(this.decode(json)) seems like we are doing a recursive call here! (this.decode)
    // WE NEED TO USE A SELF REFERENCE
    // }

    // def map[To](update: A => To): JsonDecoder[To] = // why this works without the self reference
    //   (json: Json) => update(decode(json))
  }

  val intDecoder: JsonDecoder[Int] = new JsonDecoder[Int] {
    def decode(json: Json): Int = json.value.toInt
  }

  val stringDecoder: JsonDecoder[String] = new JsonDecoder[String] {
    def decode(json: Json): String =
      if (json.value.startsWith("\"") && json.value.endsWith("\"")) // check it starts and ends with `"`
        json.value.substring(1, json.value.length - 1)
      else
        throw new IllegalArgumentException(s"${json.value} is not a valid JSON string")
  }

  // SAM syntax for JsonDecoder
  val intDecoderSAM: JsonDecoder[Int] =
    (json: Json) => json.value.toInt

  // 3a. Implement `userIdDecoder`, a `JsonDecoder` for the `UserId` case class
  // such as userIdDecoder.decode("1234") == UserId(1234)
  // but     userIdDecoder.decode("hello") would throw an Exception
  case class UserId(value: Int)
  lazy val userIdDecoder: JsonDecoder[UserId] = 
    intDecoder.map(UserId)

  // 3b. Implement `localDateDecoder`, a `JsonDecoder` for `LocalDate`
  // such as localDateDecoder.decode("\"2020-03-26\"") == LocalDate.of(2020,3,26)
  // but     localDateDecoder.decode("2020-03-26") would throw an Exception
  // and     localDateDecoder.decode("hello") would throw an Exception
  // Note: You can parse a `LocalDate` using `LocalDate.parse` with a java.time.format.DateTimeFormatter
  // e.g. DateTimeFormatter.ISO_LOCAL_DATE
  lazy val localDateDecoder: JsonDecoder[LocalDate] =
    stringDecoder.map(date => LocalDate.parse(date, DateTimeFormatter.ISO_LOCAL_DATE))

  // 3c. Implement `map` a generic function that converts a `JsonDecoder` of `From`
  // into a `JsonDecoder` of `To`.
  // Bonus: Can you re-implement `userIdDecoder` and `localDateDecoder` using `map`
  // def map[From, To](decoder: JsonDecoder[From])(update: From => To): JsonDecoder[To] =
  //   (json: Json) => update(decoder.decode(json))

  // 3d. Move `map` inside of `JsonDecoder` trait so that we can use the syntax
  // `intDecoder.map(_ + 1)` instead of `map(intDecoder)(_ + 1)`

  // 3e. Imagine we have to integrate with a weird JSON API where dates are sometimes encoded
  // using a String with the format "yyyy-mm-dd" and sometimes they are encoded using
  // JSON numbers representing the number of days since the epoch. For example,
  // weirdLocalDateDecoder.decode("\"2020-03-26\"") == LocalDate.of(2020,3,26)
  // weirdLocalDateDecoder.decode("18347")          == LocalDate.of(2020,3,26)
  // but weirdLocalDateDecoder.decode("hello") would throw an Exception
  // Try to think how we could extend JsonDecoder so that we can easily implement
  // other decoders that follow the same pattern.

  val longJsonDecoder: JsonDecoder[Long] = _.value.toLong
  val longLocalDateJsonDecoder: JsonDecoder[LocalDate] = longJsonDecoder.map(LocalDate.ofEpochDay)
  lazy val weirdLocalDateDecoder: JsonDecoder[LocalDate] = new JsonDecoder[LocalDate] {
    override def decode(json: Json): LocalDate = { // this was not the idea, the idea was to come up with or else (fallback)
      json match {
        case JsonInt(value) => longLocalDateJsonDecoder.decode(json) 
        case JsonString(value) => localDateDecoder.decode(json)
      }
    }
  }

  //////////////////////////////////////////////
  // Bonus question (not covered by the video)
  //////////////////////////////////////////////

  // 3f. How would you define and implement a `JsonDecoder` for a generic `Option`?
  // such as we can decode:
  // * "1" into a Some(1)
  // * "\"2020-08-03\"" into a Some(LocalDate.of(2020,08,3))
  // * "\"null\"" into a Some("null")
  // * "null" into "None"
  // Note: you may need to change the function signature
  def optionDecoder[A]: JsonDecoder[Option[A]] =
    ???

  // 3g. `JsonDecoder` currently throws an exception if the input is not a valid JSON.
  // How could you change the API so that it doesn't happen anymore?

}
