package part06_patterns

import akka.actor.{
  Actor,
  ActorLogging,
  ActorRef,
  ActorSystem,
  Cancellable,
  FSM,
  Props
}
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.wordspec.AnyWordSpecLike

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.language.postfixOps

class FSMSpec
    extends TestKit(ActorSystem("fsm-spec"))
    with ImplicitSender
    with AnyWordSpecLike
    with BeforeAndAfterAll {

  import FSMSpec._

  override def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  "A VendingMachine" should {
    runTestSuite(Props[VendingMachine])
  }

  "A VendingMachineFSM" should {
    runTestSuite(Props[VendingMachineFSM])
  }

  def runTestSuite(props: Props): Unit = {
    "error when not initialized" in {
      val vendingMachine = system.actorOf(props)

      vendingMachine ! VendingMachine.RequestProduct("Cola")

      expectMsg(VendingMachine.VendingError("MachineNotInitialized"))
    }

    "report a product not available" in {
      val vendingMachine = system.actorOf(props)

      vendingMachine ! VendingMachine.Initialize(
        Map("Cola" -> 2, "Sprite" -> 0),
        Map("Cola" -> 2, "Sprite" -> 2)
      )
      expectNoMessage()

      vendingMachine ! VendingMachine.RequestProduct("Milk")
      expectMsg(VendingMachine.VendingError("ProductNotAvailable"))

      vendingMachine ! VendingMachine.RequestProduct("Sprite")
      expectMsg(VendingMachine.VendingError("ProductNotAvailable"))
    }

    "throw ReceiveTimeout if money not inserted" in {
      val vendingMachine = system.actorOf(props)

      vendingMachine ! VendingMachine.Initialize(
        inventory = Map("Cola" -> 2),
        prices = Map("Cola" -> 3)
      )
      expectNoMessage()

      vendingMachine ! VendingMachine.RequestProduct("Cola")
      expectMsg(VendingMachine.Instruction("Please insert 3 dollars"))

      within(2 seconds) {
        expectMsg(VendingMachine.VendingError("RequestTimeout"))
      }
    }

    "handle the reception of partial money" in {
      val vendingMachine = system.actorOf(props)

      vendingMachine ! VendingMachine.Initialize(
        inventory = Map("Cola" -> 2),
        prices = Map("Cola" -> 3)
      )
      expectNoMessage()

      vendingMachine ! VendingMachine.RequestProduct("Cola")
      expectMsg(VendingMachine.Instruction("Please insert 3 dollars"))

      vendingMachine ! VendingMachine.ReceiveMoney(1)
      expectMsg(VendingMachine.Instruction("Please insert 2 dollars"))

      within(2 seconds) {
        expectMsg(VendingMachine.VendingError("RequestTimeout"))
        expectMsg(VendingMachine.GiveBackChange(1))
      }
    }

    "deliver product that was fully payed" in {
      val vendingMachine = system.actorOf(props)

      vendingMachine ! VendingMachine.Initialize(
        inventory = Map("Cola" -> 2),
        prices = Map("Cola" -> 3)
      )
      expectNoMessage()

      vendingMachine ! VendingMachine.RequestProduct("Cola")
      expectMsg(VendingMachine.Instruction("Please insert 3 dollars"))

      vendingMachine ! VendingMachine.ReceiveMoney(3)
      expectMsg(VendingMachine.Deliver("Cola"))
    }

    "deliver product that was overpay and return change" in {
      val vendingMachine = system.actorOf(props)

      vendingMachine ! VendingMachine.Initialize(
        inventory = Map("Cola" -> 2),
        prices = Map("Cola" -> 3)
      )
      expectNoMessage()

      vendingMachine ! VendingMachine.RequestProduct("Cola")
      expectMsg(VendingMachine.Instruction("Please insert 3 dollars"))

      vendingMachine ! VendingMachine.ReceiveMoney(2)
      expectMsg(VendingMachine.Instruction("Please insert 1 dollars"))

      vendingMachine ! VendingMachine.ReceiveMoney(2)
      expectMsg(VendingMachine.Deliver("Cola"))
      expectMsg(VendingMachine.GiveBackChange(1))
    }
  }
}

object FSMSpec {

  /**
    * Vending machine
    */
  class VendingMachine extends Actor with ActorLogging {
    import VendingMachine._

    implicit val executionContext: ExecutionContext = context.dispatcher

    override def receive: Receive = idle

    def idle: Receive = {
      case Initialize(inventory, prices) =>
        context.become(operational(inventory, prices))
      case _ => sender() ! VendingError("MachineNotInitialized")
    }

    def operational(
        inventory: Map[String, Int],
        prices: Map[String, Int]
    ): Receive = {
      case RequestProduct(product) =>
        inventory.get(product) match {
          case None | Some(0) => sender() ! VendingError("ProductNotAvailable")
          case Some(_) =>
            val price = prices(product)
            sender() ! Instruction(s"Please insert $price dollars")
            context.become(
              waitForMoney(
                inventory,
                prices,
                product,
                money = 0,
                startReceiveMoneyTimeoutScheduler,
                sender()
              )
            )
        }
    }

    def waitForMoney(
        inventory: Map[String, Int],
        prices: Map[String, Int],
        product: String,
        money: Int,
        moneyReceiveTimeoutSchedule: Cancellable,
        requester: ActorRef
    ): Receive = {
      case ReceiveMoneyTimeout =>
        requester ! VendingError("RequestTimeout")
        if (money > 0) requester ! GiveBackChange(money)
        context.become(operational(inventory, prices))
      case ReceiveMoney(amount) =>
        moneyReceiveTimeoutSchedule.cancel()
        val price = prices(product)
        if (amount + money >= price) {
          requester ! Deliver(product)

          if (amount + money - price > 0)
            requester ! GiveBackChange(amount + money - price)
          val quantityUpdated = inventory(product) - 1
          val inventoryUpdated = inventory + (product -> quantityUpdated)
          context.become(operational(inventoryUpdated, prices))
        } else {
          val remainingMoney = price - money - amount
          requester ! Instruction(s"Please insert $remainingMoney dollars")
          context.become(
            waitForMoney(
              inventory,
              prices,
              product,
              money + amount,
              startReceiveMoneyTimeoutScheduler,
              requester
            )
          )
        }
    }

    def startReceiveMoneyTimeoutScheduler: Cancellable =
      context.system.scheduler.scheduleOnce(1 second) {
        self ! ReceiveMoneyTimeout
      }
  }

  object VendingMachine {
    final case class Initialize(
        inventory: Map[String, Int],
        prices: Map[String, Int]
    )
    final case class RequestProduct(product: String)

    final case class Instruction(instruction: String)
    final case class ReceiveMoney(amount: Int)
    final case class Deliver(product: String)
    final case class GiveBackChange(amount: Int)

    final case class VendingError(reason: String)
    case object ReceiveMoneyTimeout
  }

  /**
    * How to using FSM:
    * Step 1. Define the states and the data of the actor
    */
  class VendingMachineFSM
      extends FSM[VendingMachineFSM.State, VendingMachineFSM.Data] {

    import VendingMachine._
    import VendingMachineFSM._

    implicit val executionContext: ExecutionContext = context.dispatcher

    startWith(Idle, Uninitialized)

    when(Idle) {
      case Event(Initialize(inventory, prices), Uninitialized) =>
        goto(Operational) using Initialized(inventory, prices)
      case _ =>
        sender() ! VendingError("MachineNotInitialized")
        stay()
    }

    when(Operational) {
      case Event(RequestProduct(product), Initialized(inventory, prices)) =>
        inventory.get(product) match {
          case None | Some(0) =>
            sender() ! VendingError("ProductNotAvailable")
            stay()
          case Some(_) =>
            val price = prices(product)
            sender() ! Instruction(s"Please insert $price dollars")
            goto(WaitForMoney) using WaitForMoney(
              inventory,
              prices,
              product,
              money = 0,
              sender()
            )
        }
    }

    when(WaitForMoney, stateTimeout = 1 second) {
      case Event(
            StateTimeout,
            WaitForMoney(inventory, prices, _, money, requester)
          ) =>
        requester ! VendingError("RequestTimeout")
        if (money > 0) requester ! GiveBackChange(money)
        goto(Operational) using Initialized(inventory, prices)
      case Event(
            ReceiveMoney(amount),
            WaitForMoney(
              inventory,
              prices,
              product,
              money,
              requester
            )
          ) =>
        val price = prices(product)
        if (amount + money >= price) {
          requester ! Deliver(product)

          if (amount + money - price > 0)
            requester ! GiveBackChange(amount + money - price)
          val quantityUpdated = inventory(product) - 1
          val inventoryUpdated = inventory + (product -> quantityUpdated)
          goto(Operational) using Initialized(inventoryUpdated, prices)
        } else {
          val remainingMoney = price - money - amount
          requester ! Instruction(s"Please insert $remainingMoney dollars")
          stay() using WaitForMoney(
            inventory,
            prices,
            product,
            money + amount,
            requester
          )
        }
    }

    whenUnhandled {
      case Event(_, _) =>
        sender() ! VendingError("CommandNotFound")
        stay()
    }

    onTransition {
      case stateA -> stateB =>
        log.info(s"Transitioning state from $stateA to $stateB")
    }

    initialize()
  }

  object VendingMachineFSM {
    trait State
    case object Idle extends State
    case object Operational extends State
    case object WaitForMoney extends State

    trait Data
    case object Uninitialized extends Data
    final case class Initialized(
        inventory: Map[String, Int],
        prices: Map[String, Int]
    ) extends Data
    final case class WaitForMoney(
        inventory: Map[String, Int],
        prices: Map[String, Int],
        product: String,
        money: Int,
        requester: ActorRef
    ) extends Data
  }
}
