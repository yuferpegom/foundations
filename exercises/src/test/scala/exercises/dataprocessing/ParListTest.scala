package exercises.dataprocessing

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import TemperatureExercises._

import scala.concurrent.ExecutionContext
import org.scalacheck.Arbitrary


// NOTE: Scalacheck tries to find the simple input that breaks the test: SHRINKING
class ParListTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks with ParListTestInstances {

  // test()

  test("minSampleByTemperature example") {
    val samples = List(
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 50),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 56.3),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 23.4),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 89.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 22.1),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 34.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 99.0),
    )
    val parSamples = ParList.byPartitionSize(3, samples)

    assert(
      minSampleByTemperature(parSamples) ==
        Some(Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 22.1))
    )
  }

  test("minSampleByTemperature returns the coldest Sample") {
    forAll { (samples: List[Sample]) =>
      val parSamples = ParList.byPartitionSize(3, samples)

      for {
        coldest <- minSampleByTemperature(parSamples)
        sample  <- samples
      } assert(coldest.temperatureFahrenheit <= sample.temperatureFahrenheit)
    }
  }

  test("minsSample returns the first element in an ordered by temp list") {
    forAll { (samples: List[Sample]) =>
      val ordered = samples.sortWith(_.temperatureFahrenheit <= _.temperatureFahrenheit)
      val parSamples = ParList.byPartitionSize(3, samples)
      assert(minSampleByTemperature(parSamples) === ordered.headOption)

    }

  }

  // This is called a test oracle, we are using the min method in list
  // Useful to check if a paralell implementation produces the same result as the sequencial one
  test("Min sample is consistent with std min") {
    forAll { (samples: List[Sample]) =>
      val parSamples = ParList.byPartitionSize(3, samples)
      assert(samples.minByOption(_.temperatureFahrenheit) === minSampleByTemperature(parSamples))
    }
  }

  // test("size is consistent with List size") {
  //   forAll { (numbers: ParList[Sample]) =>
  //     assert(numbers.size === numbers.toList.size)

  //   }
  // }

  test("averageTemperature example") {
    val samples = List(
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 50),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 56.3),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 23.4),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 89.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 22.1),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 34.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 99.0),
    )
    val parSamples = ParList.byPartitionSize(3, samples)

    assert(averageTemperature(parSamples) == Some(53.6))
  }

  test("Average processed in parallel is the same as getting the flattened list and processing sequencillay") {
    forAll { (samples: ParList[Sample]) =>
      val list = samples.toList
      val size = list.size
      val average = if(size == 0) None else Option(list.map(_.temperatureFahrenheit).sum / size )
      assert(average == averageTemperature(samples))
    }
  }

  test("If the average is multiplied by a factor then it is the same as multiplying the input values by that same factor") {
    forAll { (samples: ParList[Sample]) =>
      averageTemperature(samples) match {
        case None => assert(samples.toList.isEmpty)
        case Some(average) => 
          val doubled = samples.map(sample => sample.copy(temperatureFahrenheit = sample.temperatureFahrenheit * 2))
          val doubledAverage = averageTemperature(doubled)
          // we might lose presition info when transforming doubles so eventually this assert will fail
          // assert(Option(average * 2) == doupledAverage)
          // better to allow some difference
          doubledAverage match {
            case None => fail("should not happen")
            case Some(newAvg) =>
              assert((average * 2 - newAvg).abs < 0.00001)
          }
      }
    }
  }

  test("mono fold left is consisten with sum") {
    forAll { (values: ParList[Int]) =>
      assert(values.monoFoldLeft(0)(_ + _) == values.toList.sum)

    }
  }

  test("mono fold left with monoid is consisten with sum") {
    forAll { (values: ParList[Int]) =>
      assert(values.monoFoldLeft(Monoid.sumInt) == values.toList.sum)

    }
  }

  checkMonoidProperties("int sum", Monoid.sumInt)
  checkMonoidProperties("double sum", Monoid.sumDouble)
  checkMonoidProperties("int product", Monoid.timesInt)

  def checkMonoidProperties[A: Arbitrary](name: String, monoid: Monoid[A]) = {
    test(s"The monoid $name combined with its default value is a no op") {
      forAll { (value: A) =>
        assert(monoid.combine(value, monoid.default) == value)
        assert(monoid.combine(monoid.default, value) == value)
      }
    }
    test(s"The monoid $name is associative") {
      forAll { (value: A, value1: A, value2: A) =>
        assert(monoid.combine(monoid.combine(value, value1), value2) == monoid.combine(value, monoid.combine(value1, value2)))
      }
    }
  }

  // This test does not work as the default value for the combine function makes it to get wrong calcultation as it
  // is different to fold each partition and then fold the complete list to fold a flattened list.
  // For this to work the combine function must obey certain rules, check monoFoldLeft for more info.
  // The combine function and the default value must be chosen carefully
  // test("Mono fold left is consistent with list fold left") {

  //   forAll { (samples: ParList[Int], default: Int, combine:  (Int, Int) => Int) =>
  //     assert(samples.monoFoldLeft(default)(combine) == samples.toList.foldLeft(default)(combine))

  //   }
  // }

  ignore("summary is consistent between implementations") {
    forAll { (samples: ParList[Sample]) =>
      val samplesList = samples.partitions.flatten
      val reference   = summaryList(samples.partitions.flatten)
      List(
        summaryListOnePass(samplesList),
        summaryParList(samples),
        summaryParListOnePass(samples),
      ).foreach { other =>
        assert(reference.size == other.size)
        assert((reference.sum - other.sum).abs < 0.00001)
        assert(reference.min == other.min)
        assert(reference.max == other.max)
      }
    }
  }

  ignore("parFoldMap is consistent with foldMap") {
    forAll { (samples: ParList[String], update: String => Int) => // Update is not necessary we could have used identity as the signature of foldLeft ensures we are using the function
      assert(samples.foldMap(update)(Monoid.sumInt)  ==  samples.parFoldMap(update)(Monoid.sumInt))
    }
  }
}
