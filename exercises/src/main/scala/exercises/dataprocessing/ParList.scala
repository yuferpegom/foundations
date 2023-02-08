package exercises.dataprocessing

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.annotation.tailrec

/**
 * We'd like to partition the data so it is processed by different cores
 * How to choose the right partition strategy?
 * - If one partition is larget that other one core will be doing more work
 * - If many partitions cores might have equall load but there will be lot of cycles for managment:
     dispaching and load the data
 * Not all the task might require the same power of processing
 * We'd need to benchmark and configure
 * SCALE TO MORE THAN ONE COMPUTER
 * - Resilience
 * - Network
 * - Recovery
 * - Undo (sagas)
 * PARALELL MUST PRODUCE THE SAME RESULTS THAN RUNNING SEQUENTIALLY
 * 
 * 
**/
// For example, here is a ParList[Int] with two partitions:
// ParList(
//  List(1,2,3,4,5,6,7,8), // partition 1
//  List(9,10)             // partition 2
// )
// Note that we used the `apply` method with a varargs argument.
case class ParList[A](partitions: List[List[A]]) {
  def map[To](update: A => To): ParList[To] = {
    ParList(partitions.map(_.map(update)))
  }

  // Fold left take only one combine function, for our excersice we need 2 combine functions
  // We could change the signarture of the function to recevie 2 combine functions
  // This wont work well for other scenarios
  // Basically this is NOT fold left but a monoFoldLeft, a less powerfull version
  // We need a way to make clear to the user of our function that our default value and combine function
  // must be selected in a way that they comply some rules.
  // Check the bellow implementation of monoFoldLeft for the answer
  def monoFoldLeft(default: A)(combine: (A, A) => A): A = {
    partitions.map(_.foldLeft(default)(combine)).foldLeft(default)(combine)
  }

  // Step 1: Fold each partition into a single value.
  // Step 2: Fold the results of all partitions together.
  // For example,
  // Partition 1: List(a1, b1, c1, d1, e1, f1) ->       x   (folded partition 1)  \
  // Partition 2: List(a2, b2, c2, d2, e2, f2) ->       y   (folded partition 2) - z (final result)
  // Partition 3:                          Nil -> default (partition 3 is empty)  /
  // MONO = Monomorphic!!!
  // Mono fold left has a big constraint: 
  // For sum it only works when the default value is 0
  // Generalizing it means that the combine function must obey:
  // - combine(x, default) == x
  // - comgine(default, x) == x  
  // So the combine function must be used with at default value that makes it a no op
  def monoFoldLeft(monoid: Monoid[A]): A =
    partitions
      .map(_.foldLeft(monoid.default)(monoid.combine))
      .foldLeft(monoid.default)(monoid.combine)

  def toList: List[A] =
    partitions.flatten

  def size: Int =
    parFoldMap(_ => 1)(Monoid.sumInt)

  /**
   * mapReduce is known as foldMap
   * @param update
   * @param monoid
   * @tparam To
   * @return
   */
  def foldMap[To](update: A => To)(monoid: Monoid[To]): To =
    partitions
      .map{ partition =>
        partition.foldLeft(monoid.default)((state: To, value: A) => monoid.combine(state, update(value)))}
      .foldLeft(monoid.default)(monoid.combine)

  def parFoldMap[To](update: A => To)(monoid: Monoid[To]): To = {
    def foldPartition(partition: List[A]) =
        partition.foldLeft(monoid.default)((state: To, value: A) => monoid.combine(state, update(value)))

    partitions.map(foldPartition)
    .foldLeft(monoid.default)(monoid.combine)
  }

  /**
   * thisworks but the new one does it mapping only once
   *
   */
  //    map(update)
//      .monoFoldLeft(monoid)
}

object ParList {
  // The `*` at the end of List[A] is called a varargs. It means we can put as many arguments
  // as we want of type List[A] and the Scala compiler will automatically packs these arguments
  // into a collection.
  // For example, ParList(List(1,2), List(3,4)) == ParList(List(List(1,2), List(3,4)))
  // This is why we can create a List using the syntax List(1,2,3) instead of 1 :: 2 :: 3 :: Nil
  def apply[A](partitions: List[A]*): ParList[A] =
    ParList(partitions.toList)

  // Creates a ParList by grouping a List into partitions of fixed size.
  // If the length of input list is not divisible by the partition size, then
  // the last partition will be smaller. For example:
  // byPartitionSize(3, List(1,2,3,4,5,6,7,8,9,10)) == ParList(
  //   List(1,2,3),
  //   List(4,5,6),
  //   List(7,8,9),
  //   List(10)
  // )
  def byPartitionSize[A](partitionSize: Int, items: List[A]): ParList[A] =
    if (items.isEmpty) ParList()
    else ParList(items.grouped(partitionSize).toList)

}
