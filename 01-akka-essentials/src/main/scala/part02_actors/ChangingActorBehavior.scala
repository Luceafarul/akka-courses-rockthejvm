package part02_actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import part02_actors.ChangingActorBehavior.Mom.Start

object ChangingActorBehavior extends App {
  class FussyKid extends Actor {
    import Mom._
    import FussyKid._

    var state = HAPPY

    override def receive: Receive = {
      case Food(VEGETABLE) => state = SAD
      case Food(CHOCOLATE) => state = HAPPY
      case Ask(message) =>
        println(s"[${self.path.name}] receive question: $message")
        if (state == HAPPY) sender() ! KidAccept
        else sender() ! KidReject
    }
  }

  object FussyKid {
    case object KidAccept
    case object KidReject

    val HAPPY = "Happy"
    val SAD = "Sad"
  }

  class Mom extends Actor {
    import Mom._
    import FussyKid._

    override def receive: Receive = {
      case Start(kidRef) =>
        kidRef ! Food(VEGETABLE)
        kidRef ! Ask("Do you want to play?")
      case KidAccept => println(s"[${self.path.name}] Yay, my kid is happy!")
      case KidReject => println(s"[${self.path.name}] My kid is sad, but as he's healthy!")
    }
  }

  object Mom {
    final case class Start(kidRef: ActorRef)
    final case class Food(food: String)
    final case class Ask(message: String)

    val VEGETABLE = "Veggies"
    val CHOCOLATE = "Chocolate"
  }

  val system = ActorSystem("changing-actor-behavior")
  val fussyKid = system.actorOf(Props[FussyKid], "kid")
  val mom = system.actorOf(Props[Mom], "mom")

  mom ! Start(fussyKid)
}
