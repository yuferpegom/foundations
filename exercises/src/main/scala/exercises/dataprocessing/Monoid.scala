package exercises.dataprocessing

trait Monoid[A] {

  def default: A

  def combine(first: A, second: A): A

}

object Monoid {

  def sum[A](implicit numeric: Numeric[A]): Monoid[A] = new Monoid[A] {
    override def default: A = numeric.zero

    override def combine(first: A, second: A): A = numeric.plus(first, second)
  }
  val sumInt: Monoid[Int] = sum
  val sumDouble: Monoid[Double] = sum
  
  def times[A](implicit numeric: Numeric[A]): Monoid[A] = new Monoid[A] {
    override def default: A = numeric.one

    override def combine(first: A, second: A): A = numeric.times(first, second)
  }

  val timesInt: Monoid[Int] = times

  def zip[A, B](monoidA: Monoid[A], monoidB: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    override def default: (A, B) = (monoidA.default, monoidB.default)

    override def combine(first: (A, B), second: (A, B)): (A, B) = {
      (monoidA.combine(first._1, second._1), monoidB.combine(first._2, second._2))
    }
  }

  val sumIntDoubleTuple: Monoid[(Double, Int)] = zip(sumDouble, sumInt)

  def compareSample[A](compare: (A, A) => A): Monoid[Option[A]] = new Monoid[Option[A]] {
    override def default: Option[A] = Option.empty[A]

    override def combine(first: Option[A], second: Option[A]): Option[A] = (first, second) match {
      case (None, None) => None
      case (Some(sample), None) => Some(sample)
      case (None, Some(sample)) => Some(sample)
      case (Some(minSample), Some(sample)) => Some(compare(minSample, sample))

    }
  }

  val minSample: Monoid[Option[Sample]] = {
    compareSample { (sample1, sample2) =>
      if(sample1.temperatureFahrenheit < sample2.temperatureFahrenheit)
        sample1
      else sample2
    }
  }

  val maxSample: Monoid[Option[Sample]] = compareSample { (sample1, sample2) =>
    if(sample1.temperatureFahrenheit > sample2.temperatureFahrenheit)
      sample1
    else sample2
  }

  val summary: Monoid[Summary] = new Monoid[Summary] {
    override def default: Summary = Summary(None, None, 0.0, 0)

    override def combine(first: Summary, second: Summary): Summary = {
      Summary(
        min = minSample.combine(first.min, second.min),
        max = maxSample.combine(first.max, second.max),
        sum = sumDouble.combine(first.sum, second.sum),
        size = sumInt.combine(first.size, second.size)
      )
    }
  }
}
