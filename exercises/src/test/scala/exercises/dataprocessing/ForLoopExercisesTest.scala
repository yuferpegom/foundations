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

  ignore("min") {
    assert(min(List(2, 5, 1, 8)) == Some(1))
    assert(min(Nil) == None)
  }

  ignore("wordCount") {
    assert(wordCount(List("Hi", "Hello", "Hi")) == Map("Hi" -> 2, "Hello" -> 1))
    assert(wordCount(Nil) == Map.empty)
  }

}
