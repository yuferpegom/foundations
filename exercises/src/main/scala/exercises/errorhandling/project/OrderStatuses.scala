package exercises.errorhandling.project

import exercises.errorhandling.NEL

import java.time.{Duration, Instant}

sealed trait OrderStatus

case class Draft(basket: List[Item]) extends OrderStatus
case class Checkout(basket: NEL[Item], deliveryAddress: Option[Address]) extends OrderStatus
case class Submitted(basket: NEL[Item], deliveryAddress: Address, submittedAt: Instant) extends OrderStatus
case class Delivered(basket: NEL[Item], deliveryAddress: Address, submittedAt: Instant, deliveredAt: Instant) extends OrderStatus {
  def deliveryDuration = Duration.between(submittedAt, deliveredAt)
}
