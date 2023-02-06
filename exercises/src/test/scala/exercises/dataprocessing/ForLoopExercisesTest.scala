package exercises.dataprocessing

import exercises.dataprocessing.ForLoopExercises._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ForLoopExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  test("sum") {
    assert(sum(List(1, 5, 2)) == 8)
    assert(sum(Nil) == 0)
  }

  test("sum is consistent with List sum") {
    forAll { (numbers: List[Int]) =>
      assert(sum(numbers) == numbers.sum)
    }
  }

  test("size") {
    assert(size(List(2, 5, 1, 8)) == 4)
    assert(size(Nil) == 0)
  }

  test("size is consistent with List size") {
    forAll { (numbers: List[Int]) =>
      assert(size(numbers) == numbers.size)
      if(numbers.isEmpty)
        assert(size(numbers) == 0)
    }
  }

  test("size is consistent with List concatenation") {
    forAll { (numbers: List[Int], numbers1: List[Int]) =>
      val totalConcatenated = numbers1.concat(numbers)
      assert(size(numbers) + size(numbers1) == size(totalConcatenated))
      assert(size(totalConcatenated) == numbers1.size + numbers.size)
    }
  }

  test("min") {
    assert(min(List(2, 5, 1, 8)) == Some(1))
    assert(min(Nil) == None)
  }

  test("min is consistent with list min function") {
    forAll { (numbers: List[Int]) =>
      if(numbers.isEmpty) {
        assert(min(numbers) == Option.empty)
      }
      else {
        assert(numbers.min == min(numbers).get)
      }
    }
  }

  test("min should be the min of all numbers in the list") {
    forAll { (numbers: List[Int]) =>
      min(numbers) match {
        case Some(minNumber) => numbers.foreach(number => assert(minNumber <= number))
        case None => succeed 
      }

    }
  }
  
  test("min returns a value that belongs to the input list") {
    forAll { (numbers: List[Int]) =>
      min(numbers).foreach(number => assert(numbers.contains(number)))
    }
  }

  test("wordCount") {
    assert(wordCount(List("Hi", "Hello", "Hi")) == Map("Hi" -> 2, "Hello" -> 1))
    assert(wordCount(Nil) == Map.empty)
  }

  test("foldLeft process inputs in order") {
    forAll{ (numbers: List[Int]) =>
      val result  = foldLeft(numbers, List.empty[Int])( (state, number) =>  state :+ number)
      assert(result == numbers)
    }
  }

  test("consisten with the list map function") {
    forAll{ (numbers: List[Int], f: Int => Int) =>
      val result = map(numbers)(f)
      assert(numbers.map(f) == result)
    }
  }

  test("return the last optional if some") {
    val input = List(2, 3, 1)
    val resutl = lastOption(input)
    assert(resutl === Some(input.last))
    val result1 = lastOption(List.empty)
    assert(result1 == None)
  }

  test("lastOption consistent with List lastOption") {
    forAll { (numbers: List[Int]) =>
      assert(lastOption(numbers) == numbers.lastOption)
    }
  }

  test("generalMin consistent with List minOption") {
    forAll { (numbers: List[Int]) =>
      assert(generalMin(numbers)(implicitly) == numbers.minOption)
    }
  }
}
