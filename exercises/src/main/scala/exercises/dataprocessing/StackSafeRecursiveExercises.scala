package exercises.dataprocessing

import scala.annotation.tailrec

object StackSafeRecursiveExercises {

  def unsafeSum(numbers: List[Int]): Int =
    numbers match {
      case Nil          => 0
      case head :: tail => head + unsafeSum(tail)
    }

  def sum(numbers: List[Int]): Int = {
    // @tailrec
    // def go(numbers: List[Int], accumulator: Int): Int =
    //   numbers match {
    //     case Nil          => accumulator
    //     case head :: tail => go(tail, accumulator + head)
    //   }
    // go(numbers, 0)
    foldLeft(numbers, 0)(_ + _)
  }

  // a. Implement `min` using a recursion
  // such as min(List(2,5,1,8)) == Some(1)
  // and     min(Nil) == None
  def min(numbers: List[Int]): Option[Int] = {
    // @tailrec
    // def go(input: List[Int], state: Option[Int]): Option[Int] = {
    //   input match {
    //     case head :: next => 
    //       val min = state match {
    //         case None => 
    //           Option(head)
    //         case Some(s) => Option(s min head)
    //       }
    //       go(next, min)
    //     case Nil => state 
    //   }
    // }
    // go(numbers, None)
    foldLeft(numbers, Option.empty[Int]){(state, number) => 
      state match {
        case None => Option(number)
        case Some(s) => Option(s min number)
      }
    }
  }

  // b. Implement `reverse` using a recursion
  // such as reverse(List(2,5,1,8)) == List(8,1,5,2)
  // and     reverse(Nil) == Nil
  // Note: Ensure size is stack-safe
  def reverse[A](items: List[A]): List[A] = {
    // @tailrec
    // def go(input: List[A], state: List[A]): List[A] = {
    //   input match {
    //     case head :: next => go(next, head +: state)
    //     case Nil => state
    //   }
    // }
    // go(items, List.empty)
    foldLeft(items, List.empty[A]){(state, item) => item +: state}
  }
  // c. Implement `foldLeft` using a recursion
  // Note: Ensure size is stack-safe
  def foldLeft[From, To](items: List[From], default: To)(combine: (To, From) => To): To = {
    items match {
      case head :: next => foldLeft(next, combine(default, head))(combine)
      case Nil => default
    }
  }

}
