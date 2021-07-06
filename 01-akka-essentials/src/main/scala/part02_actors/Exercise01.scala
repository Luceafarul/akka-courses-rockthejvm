package part02_actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object Exercise01 extends App {

  /**
    * 1. A Counter actor
    * - Increment
    * - Decrement
    * - Print
    *
    * 2. A Bank Account as an actor
    * - Deposit an amount
    * - Withdraw an amount
    * - Statement
    * Replies with:
    * - Success
    * - Failure
    *
    * Interact with some other kind of actor
    */
  class Counter extends Actor {
    import Counter._
    private var count = 0

    override def receive: Receive = {
      case Increment => count += 1
      case Decrement => count -= 1
      case Print     => println(s"Current count: $count")
    }
  }
  object Counter {
    final case object Increment
    final case object Decrement
    final case object Print
  }

  val system = ActorSystem("exercise")

  import Counter._
  val counter = system.actorOf(Props[Counter], "counter")
  for (_ <- 0 to 10) counter ! Increment
  for (_ <- 0 to 3) counter ! Decrement
  counter ! Print

  class BankAccount(val initialBalance: Int = 0) extends Actor {
    import BankAccount._
    var balance = initialBalance

    // TODO: add validation for amount < 0 in Deposit and Withdraw
    override def receive: Receive = {
      case Deposit(amount, ref) =>
        balance += amount
        ref ! Success(s"Deposit success. Balance: $balance")
      case Withdraw(amount, ref) =>
        if (amount > balance)
          ref ! Failure(s"Insufficient funds. Balance: $balance")
        else {
          balance -= amount
          ref ! Success(s"Withdraw success. Balance: $balance")
        }
      case Statement(ref) =>
        ref ! Success(s"Balance: $balance")
    }
  }
  object BankAccount {
    final case class Deposit(amount: Int, ref: ActorRef)
    final case class Withdraw(amount: Int, ref: ActorRef)
    final case class Statement(ref: ActorRef)

    final case class Success(message: String)
    final case class Failure(message: String)

    def props(initialBalance: Int): Props =
      Props(new BankAccount(initialBalance))
  }

  class BankClient(name: String) extends Actor {
    import BankAccount._

    override def receive: Receive = {
      case Success(message) => println(s"[SUCCESS] $message")
      case Failure(message) => println(s"[FAILURE] $message")
    }
  }
  object BankClient {
    def props(name: String): Props = Props(new BankClient(name))
  }

  val johnAccount = system.actorOf(BankAccount.props(1000), "john-account")
  val jessicaAccount = system.actorOf(BankAccount.props(1000), "jessica-account")
  val john = system.actorOf(BankClient.props("John"), "john")
  val jessica = system.actorOf(BankClient.props("Jessica"), "jessica")

  import BankAccount._
  jessicaAccount ! Withdraw(1001, jessica)
  jessicaAccount ! Withdraw(750, jessica)
  jessicaAccount ! Deposit(3750, jessica)
  johnAccount ! Statement(john)
}
