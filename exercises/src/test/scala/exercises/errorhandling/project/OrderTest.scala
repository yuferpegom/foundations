package exercises.errorhandling.project

import exercises.errorhandling.NEL
import exercises.errorhandling.project.OrderError.{AddressIsRequired, EmptyBasket, InvalidStatus}
import exercises.errorhandling.project.OrderGenerator._
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.time.{Duration, Instant}

class OrderTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  test("checkout successful example") {
    val basket =  List(Item("A1", 2, 12.99))
    val order = Order(
      id = "AAA",
      status = Draft(basket),
      createdAt = Instant.now()
    )

    order.checkout match {
      case Left(value)     => fail(s"Expected success but got $value")
      case Right(newOrder) => assert(newOrder.status == Checkout(NEL(basket.head, basket.tail), None))
    }
  }

  test("checkout empty basket example") {
    val order = Order(
      id = "AAA",
      status = Draft(Nil),
      createdAt = Instant.now()
    )

    assert(order.checkout == Left(EmptyBasket))
  }

//  test("checkout invalid status example") { do not apply anymore as addres is required when deliverin
//    val nelBasket = NEL(Item("A1", 2, 12.99))
//    val order = Order(
//      id = "AAA",
//      status = Delivered(nelBasket, None),
//      deliveryAddress = None,
//      createdAt = Instant.now(),
//      submittedAt = None,
//      deliveredAt = None
//    )
//
//    assert(order.checkout == Left(InvalidStatus(Delivered(nelBasket))))
//  }

  test("submit successful example") {
    val nelBasket = NEL(Item("A1", 2, 12.99))
    val address = Address(12, "E16 8TR")
    val order = Order(
      id = "AAA",
      status = Checkout(nelBasket, Some(address)),
      createdAt = Instant.now()
    )
    val submittedAt = Instant.now()

    order.submit(submittedAt) match {
      case Left(value)     => fail(s"Expected success but got $value")
      case Right(newOrder) => assert(newOrder.status == Submitted(nelBasket, address, submittedAt))
    }
  }

  test("submit no address example") {
    val nelBasket = NEL(Item("A1", 2, 12.99))
    val order = Order(
      id = "AAA",
      status = Checkout(nelBasket, None),
      createdAt = Instant.now()
    )

    assert(order.submit(Instant.now()) == Left(AddressIsRequired))
  }

  test("submit invalid status example") {
    val nelBasket = NEL(Item("A1", 2, 12.99))
    val address = Address(12, "E16 8TR")
    val deliveredAt = Instant.now
    val submittedAt = Instant.now
    val order = Order(
      id = "AAA",
      status = Delivered(nelBasket, address, submittedAt, deliveredAt),
      createdAt = Instant.now()
    )

    assert(order.submit(Instant.now()) == Left(InvalidStatus(Delivered(nelBasket, address, submittedAt, deliveredAt))))
  }

//  test("submit empty basket example") { // Don't apply anymore as the model won't allow to represent this state
//    val order = Order(
//      id = "AAA",
//      status = Checkout,
//      basket = Nil,
//      deliveryAddress = Some(Address(12, "E16 8TR")),
//      createdAt = Instant.now(),
//      submittedAt = None,
//      deliveredAt = None
//    )
//
//    assert(order.submit(Instant.now()) == Left(EmptyBasket))
//  }

  test("happy path") {
    forAll(orderIdGen, instantGen, durationGen, durationGen, nelOf(itemGen), addressGen) {
      (orderId, createdAt, submittedDelay, deliveryDelay, items, deliveryAddress) =>

      val submittedAt     = createdAt.plus(submittedDelay)
      val deliveredAt     = submittedAt.plus(deliveryDelay) // 30 hours
      val order           = Order.empty(orderId, createdAt)
      val deliveryAddress = Address(23, "E16 8FV")

      val result = for {
        order         <- order.addItems(items)
        order         <- order.checkout
        order         <- order.updateDeliveryAddress(deliveryAddress)
        order         <- order.submit(submittedAt)
        orderDuration <- order.deliver(deliveredAt)
      } yield orderDuration

      assert(
        result == Right(
          Order(
            id = orderId,
            status = Delivered(items, deliveryAddress, submittedAt, deliveredAt),
            createdAt = createdAt
          )
        )
      )

        val deliveryDuration = Duration.between(submittedAt, deliveredAt)

      assert(result.map(_.status) == Right(deliveryDuration))
    }

    }

}
