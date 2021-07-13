package part02_actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object ChildActors extends App {
  class Parent extends Actor {
    import Parent._

    override def receive: Receive = {
      case CreateChild(name) =>
        println(s"${self.path} Creating child...")
        val childRef = context.actorOf(Props[Child], name)
        context.become(withChild(childRef))
    }

    def withChild(child: ActorRef): Receive = {
      case TellChild(message) => child forward message
    }
  }
  object Parent {
    final case class CreateChild(name: String)
    final case class TellChild(message: String)
  }

  class Child extends Actor {
    override def receive: Receive = {
      case message => println(s"${self.path} I got: $message")
    }
  }

  val system = ActorSystem("parent-child-demo")
  val parent = system.actorOf(Props[Parent], "parent")

  import Parent._
  parent ! CreateChild("child")
  parent ! TellChild("Hello, child!")

  // Actor selection
  // If the path was wrong message move to dead letters
  val childSelection = system.actorSelection("/user/parent/child")
  childSelection ! "I found you!"

  // Never pass mutable actor state or the 'this' reference to child actors.
  class NaiveBankAccount extends Actor {
    import NaiveBankAccount._
    import CreditCard._

    var balance = 0

    override def receive: Receive = {
      case InitializeAccount =>
        val creditCardRef = context.actorOf(Props[CreditCard], "credit-card")
        creditCardRef ! AttachToAccount(this)
      case Deposit(amount)  => deposit(amount)
      case Withdraw(amount) => withdraw(amount)
    }

    def deposit(amount: Int): Unit = {
      println(s"${self.path} depositing $amount on top of $balance")
      balance += amount
    }
    def withdraw(amount: Int): Unit = {
      println(s"${self.path} withdrawing $amount from $balance")
      balance -= amount
    }
  }
  object NaiveBankAccount {
    final case class Deposit(amount: Int)
    final case class Withdraw(amount: Int)
    final case object InitializeAccount
  }

  class CreditCard extends Actor {
    import CreditCard._
    override def receive: Receive = {
      case AttachToAccount(account) => context.become(attachedTo(account))
    }

    def attachedTo(account: NaiveBankAccount): Receive = {
      case CheckStatus =>
        println(s"[${self.path}] Your balance: ${account.balance}")
        account.withdraw(10)
    }
  }
  object CreditCard {
    final case class AttachToAccount(bankAccount: NaiveBankAccount) // !!!!
    final case object CheckStatus
  }

  import NaiveBankAccount._
  import CreditCard._
  val bankAccount = system.actorOf(Props[NaiveBankAccount], "bank-account")
  bankAccount ! InitializeAccount

  Thread.sleep(500)
  val creditCardSelection = system.actorSelection("/user/bank-account/credit-card")
  creditCardSelection ! CheckStatus
}
