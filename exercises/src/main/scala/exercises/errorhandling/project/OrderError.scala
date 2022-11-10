package exercises.errorhandling.project

sealed trait OrderError
object OrderError {
  case object EmptyBasket                         extends OrderError
  case class InvalidStatus(currentStatus: OrderStatus) extends OrderError
  case object AddressIsRequired extends OrderError
//  case object MissingSubmittedAtTimestamp extends OrderError thans to the new design this is not required anymore
}
