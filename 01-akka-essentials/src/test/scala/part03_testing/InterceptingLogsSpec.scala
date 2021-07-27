package part03_testing

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import akka.testkit.{EventFilter, ImplicitSender, TestKit}
import com.typesafe.config.ConfigFactory
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class InterceptingLogsSpec
    extends TestKit(
      ActorSystem(
        "intercepting-logs-system",
        ConfigFactory.load("app.conf").getConfig("interceptingLogMessages")
      )
    )
    with Matchers
    with ImplicitSender
    with AnyWordSpecLike
    with BeforeAndAfterAll {

  override def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  import InterceptingLogsSpec._

  "A checkout flow" should {
    val item = "Scala Book"
    val validCreditCard = "1234-1234-1234-1234"
    val invalidCreditCard = "0000-0000-0000-0000"

    "correctly log the dispatch of an order" in {
      EventFilter
        .info(
          pattern = s"Order [0-9]+ for item $item has been dispatched",
          occurrences = 1
        )
        .intercept {
          val checkoutActor = system.actorOf(Props[CheckoutActor])
          checkoutActor ! Checkout(item, validCreditCard)
        }
    }

    "freak out if the payment is denied" in {
      EventFilter[RuntimeException](occurrences = 1).intercept {
        val checkoutActor = system.actorOf(Props[CheckoutActor])
        checkoutActor ! Checkout(item, invalidCreditCard)
      }
    }
  }
}

object InterceptingLogsSpec {
  final case class Checkout(item: String, creditCard: String)
  final case class AuthorizeCard(creditCard: String)
  final case class DispatchOrder(item: String)
  final case object OrderConfirmed
  final case object CardAccepted
  final case object CardDenied

  class CheckoutActor extends Actor {
    private val paymentManager = context.actorOf(Props[PaymentManager])
    private val fulfilmentManager = context.actorOf(Props[FulfillmentManager])

    override def receive: Receive = awaitingCheckout

    def awaitingCheckout: Receive = {
      case Checkout(item, card) =>
        paymentManager ! AuthorizeCard(card)
        context.become(paymentPending(item))
    }

    def paymentPending(item: String): Receive = {
      case CardDenied =>
        throw new RuntimeException("I can't handle this")
      case CardAccepted =>
        fulfilmentManager ! DispatchOrder(item)
        context.become(pendingFulfillment(item))
    }

    def pendingFulfillment(item: String): Receive = {
      case OrderConfirmed => context.become(awaitingCheckout)
    }
  }

  class PaymentManager extends Actor {
    override def receive: Receive = {
      case AuthorizeCard(card) =>
        if (card.startsWith("0")) sender() ! CardDenied
        else {
          Thread.sleep(4000)
          sender() ! CardAccepted
        }
    }
  }

  class FulfillmentManager extends Actor with ActorLogging {
    var orderId = 1

    override def receive: Receive = {
      case DispatchOrder(item) =>
        orderId += 1
        log.info(s"Order $orderId for item $item has been dispatched")
        sender() ! OrderConfirmed
    }
  }
}
