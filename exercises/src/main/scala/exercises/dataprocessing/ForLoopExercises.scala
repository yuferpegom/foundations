package exercises.dataprocessing

object ForLoopExercises {

   

  def sum(numbers: List[Int]): Int = {
    // var total = 0

    // for (number <- numbers)
    //   total += number

    // total
    // val f: (Int, Int) => Int = (a, b) => a + b
    foldLeft(numbers, 0)(_ + _)
  }

  // a. Implement `size` using a mutable state and a for loop
  // such as size(List(2,5,1,8)) == 4
  // and     size(Nil) == 0
  def size[A](items: List[A]): Int = {
    // var size = 0

    // for (item <- items)
    //   size += 1 

    // size 
    foldLeft(items, 0)((s, _) => s + 1)
  }

  // b. Implement `min` using a mutable state and a for loop
  // such as min(List(2,5,1,8)) == Some(1)
  // and     min(Nil) == None
  // Note: Option is an enumeration with two values:
  // * Some when there is a value and
  // * None when there is no value (a bit like null)
  def min(numbers: List[Int]): Option[Int] = {
    // var min = Option.empty[Int]
    // val f: (Int, Option[Int]) => Int = (number, min) => number min min.getOrElse(number) 
    // for (number <- numbers) {
    //   min = Some(number min min.getOrElse(number))
    // }
    // min
    foldLeft(numbers,  Option.empty[Int]) {
      (min, number) => Some(number min min.getOrElse(number))
    }
    // pattern(numbers, min, Option.empty)
  }

  // c. Implement `wordCount` using a mutable state and a for loop.
  // `wordCount` compute how many times each word appears in a `List`
  // such as wordCount(List("Hi", "Hello", "Hi")) == Map("Hi" -> 2, "Hello" -> 1)
  // and     wordCount(Nil) == Map.empty
  // Note: You can lookup an element in a `Map` with the method `get`
  // and you can upsert a value using `updated`
  def wordCount(words: List[String]): Map[String, Int] = {
    foldLeft(words, Map.empty[String, Int]) {
      (frequencies, word) => 
        val frequency = frequencies.getOrElse(word, 0)
        frequencies.updated(word, frequency + 1)
    }
  }

  // d. `sum`, `size`, `min` and `wordCount` are quite similar.
  // Could you write a higher-order function that captures this pattern?
  // How would you call it?
  def foldLeft[A, B](inputs: List[A], default: B)(op: (B, A) => B): B = {
    var acc = default
    for(input <- inputs) {
        acc = op(acc, input)
    }
    acc 
  }

  // e. Refactor `sum`, `size`, `min` and `wordCount` using the higher-order
  // function you defined above.

  //////////////////////////////////////////////
  // Bonus question (not covered by the video)
  //////////////////////////////////////////////

  // f. `foldLeft` can be used to implement most of the List API.
  // Do you want to give it a try? For example, can you implement
  // `map`, `reverse` and `lastOption` in terms of `foldLeft`
  def map[From, To](elements: List[From])(update: From => To): List[To] =
    foldLeft(elements, List.empty[To]) {
      (acc, item) => acc :+ update(item)
    }

  // reverse(List(3,8,1)) == List(1,8,3)
  // reverse(Nil) == Nil
  def reverse[A](elements: List[A]): List[A] =
    foldLeft(elements, List.empty[A])((acc, item) => item +: acc) 

  // lastOption(List(3,8,1)) == Some(1)
  // lastOption(Nil) == None
  def lastOption[A](elements: List[A]): Option[A] =
    foldLeft(elements, Option.empty[A])((_, item) => Option(item)) 

  // g. Can you generalise `min` so that it applies to more types like `Long`, `String`, ...?
  // Note: You may want to use the class Ordering from the standard library
  def generalMin[A](elements: List[A])(implicit ord: Ordering[A]): Option[A] =
    elements.sorted.headOption

  // def generalMin[A](elements: List[A])(ord: Ordering[A]): Option[A] =
  //   foldLeft(elements, Option.empty[A]) {
  //     case (None, element)        => Some(element)
  //     case (Some(state), element) => Some(ord.min(state, element))
  //   }

}
