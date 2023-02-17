package exercises.dataprocessing

import scala.concurrent.ExecutionContext

object TemperatureExercises {

  implicit val ec: ExecutionContext = ThreadPoolUtil.fixedSizeExecutionContext(4)
  // b. Implement `minSampleByTemperature` which finds the `Sample` with the coldest temperature.
  // `minSampleByTemperature` should work as follow:
  // Step 1: Find the local minimums (for each partition the `Sample` with the coldest temperature).
  // Step 2: Find the minimum value among the local minimums.
  // Note: We'll write test in the file `ParListTest.scala`
  def minSampleByTemperature(samples: ParList[Sample]): Option[Sample] = {

    // Frist implementation: Using flatMap and then fold left

    // val mins = samples.partitions.flatMap(minSampleByTemperaturePerPartitionList)
    // minSampleByTemperaturePerPartitionList(mins)

    // Secondi implementation: Using foldLeft
    // samples.foldMap(sample => Option(sample))(Monoid.minSample)

    // third implementation using par computation

    samples.parFoldMap(sample => Option(sample))(Monoid.minSample)
  }

  def minSampleByTemperaturePerPartitionList(partition: List[Sample]): Option[Sample] = {
    partition.foldLeft(Option.empty[Sample]){
      case (None, sample) => Some(sample)
      case (Some(minSample), sample) => 
        if(minSample.temperatureFahrenheit < sample.temperatureFahrenheit)
          Some(minSample)
        else Some(sample)
    }
  }

  // c. Implement `averageTemperature` which finds the average temperature across all `Samples`.
  // `averageTemperature` should work as follow:
  // Step 1: Compute the sum of all samples temperatures
  //   a) Compute the sum per partition
  //   b) Sum-up the sum of each partition
  // Step 2: Compute the size of the dataset
  //   a) Compute the size of each partition
  //   b) Sum-up the size of each partition
  // Step 3: Divide the total temperature by the size of dataset.
  // In case the input `ParList` is empty we return `None`.
  // Bonus: Can you calculate the size and sum in one go?
  def averageTemperature(samples: ParList[Sample]): Option[Double] = {
    // val temperatureAndSizePerPartition: List[(Double, Double)] = 
    //   samples
    //   .partitions
    //   .map(samples => (totalTemperaturePerPartitionList(samples.map(_.temperatureFahrenheit)), samples.size))
    // val totalSize = temperatureAndSizePerPartition.map(_._2).sum
    // val temperaturePerPartition = temperatureAndSizePerPartition.map(_._1)
    // val totalTemperature = totalTemperaturePerPartitionList(temperaturePerPartition) // THIS ACTUALLY WORKS


    // val (totalTemperature, totalSize) = samples
    //  .parFoldMap(sample => (sample.temperatureFahrenheit, 1))(Monoid.sumIntDoubleTuple)
    // if(totalSize == 0) None
    // else Some(totalTemperature / totalSize)

    // first version
    // val (totalTemperature, totalSize) = sumTuples(samples.partitions.map(sumSizePerPartition))

    // second version: Using foldLeft, this should be receivint 2 combine functions (I didn't do that``)
    // val (totalTemperature, totalSize) = samples
    //   .map(sample => (sample.temperatureFahrenheit, 1))
    //   .foldLeft((0.0, 0)) { 
    //     case ((temperatureAcc, sizeAcc), (temperature, size)) => (temperatureAcc + temperature, sizeAcc + size)
    //   }

    // Instead of the above we use the monoFoldLeft version
    // Third version: using monoFoldLeft without the monoid
    // val (totalTemperature, totalSize) = samples
    //   .map(sample => (sample.temperatureFahrenheit, 1))
    //   .monoFoldLeft((0.0, 0)) { 
    //     case ((temperatureAcc, sizeAcc), (temperature, size)) => (temperatureAcc + temperature, sizeAcc + size)
    //   }


    // Fourth version: Using map and mono fold left and the corresponding monoid  

    // val (totalTemperature, totalSize) = samples
    //   .map(sample => (sample.temperatureFahrenheit, 1))
    //   .monoFoldLeft(Monoid.sumIntDoubleTuple)

    // Fifth version: Uses foldMap (also known as map reduce)

    // val (totalTemperature, totalSize) = samples.foldMap(sample => (sample.temperatureFahrenheit, 1))(Monoid.sumIntDoubleTuple)

    // Sixth version: Uses parFoldMap (also known as map reduce)

    val (totalTemperature, totalSize) = samples.parFoldMap(sample => (sample.temperatureFahrenheit, 1))(Monoid.sumIntDoubleTuple)

    if(totalSize == 0) None
    else Some(totalTemperature / totalSize) 
  }

  def totalTemperaturePerPartitionList(temperatures: List[Double]): Double = {
    temperatures.foldLeft(0.0)((temperatureAcc, temperature) => temperatureAcc + temperature)
  }

  def sumSizePerPartition(partition: List[Sample]): (Double, Int) = {
    partition.foldLeft[(Double, Int)](0.0, 0) {
      case ((sum, size), sample) => ((sum + sample.temperatureFahrenheit), size + 1)
    }
  }

  def sumTuples(tuples: List[(Double, Int)]): (Double, Int) = {
    tuples.foldLeft[(Double, Int)]((0.0, 0)) {
      case ((sum1, size1), (sum2, size2)) => ((sum1 + sum2), (size1 + size2))
    }
  }

  def sumTemperatures(samples: ParList[Sample]): Double = {
    foldLeft(samples, 0.0)(
      combineElements = (state, sample) => state + sample.temperatureFahrenheit,
      combineIntermediateResults = _ + _
    )
  }

  /**
   * THIS is map reduce!!!!!
   * @param samples
   * @return
   */
  def sumTemperaturesMonoFoldLeft(samples: ParList[Sample]): Double = {
    samples
      .parFoldMap(_.temperatureFahrenheit)(Monoid.sumDouble)
  }

  // d. Implement `foldLeft` and then move it inside the class `ParList`.
  // `foldLeft` should work as follow:
  // Step 1: Fold each partition into a single value.
  // Step 2: Fold the intermediate results of all partitions together.
  // For example,
  // Partition 1: List(a1, b1, c1, d1, e1, f1) ->    res1 (intermediate result of partition 1) \
  // Partition 2: List(a2, b2, c2, d2, e2, f2) ->    res2 (intermediate result of partition 2) - finalResult
  // Partition 3:                          Nil -> default (partition 3 is empty)               /
  def foldLeft[From, To](parList: ParList[From], default: To)(
    combineElements: (To, From) => To, 
    combineIntermediateResults: (To, To) => To): To = {
  //   val intermediate: List[To] = parList.partitions.map(l => test(l, default)(combine))
    // val c: (To, To) => To = combine

    // Fold left cannot be implemented using this signature !!!!!!!!!!!!
    // we refactor the signature to receive 2 combine functions
    // test[To, To](intermediate, default)(c)
    parList.partitions
      .map(_.foldLeft(default)(combineElements))
      .foldLeft(default)(combineIntermediateResults)
  }

  // def test[From, To](input: List[From], default: To)(combine: (To, From) => To): To = {
  //   input.foldLeft(default)(combine)
  // }

  // e. Implement `monoFoldLeft`, a version of `foldLeft` that does not change the element type.
  // Then move `monoFoldLeft` inside  the class `ParList`.
  // `monoFoldLeft` should work as follow:
  // moved

  // `summaryList` iterate 4 times over `samples`, one for each field.
  def summaryList(samples: List[Sample]): Summary =
    Summary(
      min = samples.minByOption(_.temperatureFahrenheit),
      max = samples.maxByOption(_.temperatureFahrenheit),
      sum = samples.foldLeft(0.0)((state, sample) => state + sample.temperatureFahrenheit),
      size = samples.size
    )

  def summaryListOnePass(samples: List[Sample]): Summary =
    samples.foldLeft(
      Summary(
        min = None,
        max = None,
        sum = 0.0,
        size = 0
      )
    )(
      (state, sample) =>
        Summary(
          min = state.min.fold(Some(sample))(
            current =>
              if (current.temperatureFahrenheit <= sample.temperatureFahrenheit) Some(current)
              else Some(sample)
          ),
          max = state.max.fold(Some(sample))(
            current =>
              if (current.temperatureFahrenheit >= sample.temperatureFahrenheit) Some(current)
              else Some(sample)
          ),
          sum = state.sum + sample.temperatureFahrenheit,
          size = state.size + 1
      )
    )

  // Implement `summaryParList` by calling `parFoldMap` once for each field of Summary.
  // Note: In `ParListTest.scala`, there is already a test checking that `summaryParList`
  // should return the same result as `summaryList`
  def summaryParList(samples: ParList[Sample]): Summary =
    Summary(
      min = samples.parFoldMap(sample => Option(sample))(Monoid.minSample),
      max = samples.parFoldMap(sample => Option(sample))(Monoid.maxSample),
      sum = samples.parFoldMap(_.temperatureFahrenheit)(Monoid.sumDouble),
      size = samples.parFoldMap(_ => 1)(Monoid.sumInt)
    )

  // Implement `summaryParListOnePass` using `parFoldMap` only ONCE.
  // Note: In `ParListTest.scala`, there is already a test checking that `summaryParListOnePass`
  // should return the same result as `summaryList`
  def summaryParListOnePass(samples: ParList[Sample]): Summary = {
    samples.parFoldMap(
      sample => Summary(
        Some(sample),
        Some(sample),
        sample.temperatureFahrenheit,
        1
      )
    )(Monoid.summary)
  }

  // def sampleToOutput[A](sample: Sample, key: Sample => A ): Map[A, Summary] = {
  //   Map(
  //     key(sample) -> Summary(
  //       min = Some(sample),
  //       max = Some(sample),
  //       sum = sample.temperatureFahrenheit,
  //       size = 1
  //     )
  //   )
  // }
  // def sampleToOutputByCity(sample: Sample) = sampleToOutput(sample, _.city)

  // def monoidOutput[A]: Monoid[Map[A, Summary]] = new Monoid[Map[A, Summary]] {
  //   override def default: Map[A, Summary] = Map.empty

  //   override def combine(first: Map[A, Summary], second: Map[A, Summary]): Map[A, Summary] = {
  //     second.foldLeft(first) {
  //       case (state, (city, summary)) =>
  //         state.updatedWith(city) {
  //           case Some(currentSummary) =>
  //             Some(Monoid.summary.combine(summary, currentSummary))
  //           case None => Some(summary)
  //         }
  //     }
  //   }
  // }

  // def aggregate[A](samples: ParList[Sample], sampleToOutput: Sample => Map[A, Summary]): Map[A, Summary] = {
  //   samples.parFoldMap(sampleToOutput)(monoidOutput)
  // }


  val sampleToSummary: Sample => Summary = sample => Summary(
    min = Some(sample),
    max = Some(sample),
    sum = sample.temperatureFahrenheit,
    1
  )

  // This is the same as before only that I was tryng to
  def sampleToOutput[Key](sample: Sample, labels: Sample => List[Key] ): Map[Key, Summary] = {
    // This work but there is an easiest way
    // labels(sample).foldLeft(Map.empty[Key, Summary]) {
    //   case (summary, key) => 
    //     summary.updatedWith(key) {
    //       case Some(currentSummary) => Some(Monoid.summary.combine(sampleToSummary(sample), currentSummary))
    //       case None => Some(sampleToSummary(sample))
    //     }
    // }
    labels(sample).map(key => (key -> sampleToSummary(sample))).toMap
  }

  def monoidOutput[A]: Monoid[Map[A, Summary]] = new Monoid[Map[A, Summary]] {
    override def default: Map[A, Summary] = Map.empty

    override def combine(first: Map[A, Summary], second: Map[A, Summary]): Map[A, Summary] = {
      second.foldLeft(first) { case (state, (label, summary)) => 
        state.get(label) match {
          case Some(value) => 
            state.updated(label, Monoid.summary.combine(value, summary))
          case None => state.updated(label, summary)
        }
      }
    }
  }

  def sampleToOutputByCity(sample: Sample): Map[String, Summary] = sampleToOutput(sample, state => List(state.city))

  def aggregate[A](samples: ParList[Sample], sampleToOutput: Sample => Map[A, Summary]): Map[A, Summary] = {
    samples.parFoldMap(sampleToOutput)(monoidOutput)
  }
}
