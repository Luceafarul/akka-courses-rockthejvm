package part02_actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import part02_actors.Exercise01.{Counter, system}

object Exercise02 extends App {

  /**
    * 1. Recreate the Counter Actor, with context.become and NO MUTABLE STATE
    * - Increment
    * - Decrement
    * - Print
    *
    * 2. Simplified voting system
    * - Two kinds of actor
    * - Citizen
    * - VoteAggregator
    * Additional case classes:
    * - Vote
    * - AggregatorVotes
    * - VoteStatusRequest
    * - VoteStatusReply
    */
  class Counter extends Actor {
    import Counter._

    override def receive: Receive = state(count = 0)

    def state(count: Int): Receive = {
      case Increment => context.become(state(count + 1))
      case Decrement => context.become(state(count - 1))
      case Print     => println(s"Current count: $count")
    }
  }
  object Counter {
    final case object Increment
    final case object Decrement
    final case object Print
  }

  import Counter._

  val system = ActorSystem("exercise")

  val counter = system.actorOf(Props[Counter], "counter")
  for (_ <- 0 to 10) counter ! Increment
  for (_ <- 0 to 3) counter ! Decrement
  counter ! Print

  class Citizen extends Actor {
    import Citizen._
    import VoteAggregator._

    override def receive: Receive = vote(None)

    def vote(candidate: Option[String]): Receive = {
      case Vote(candidate)   => context.become(vote(Some(candidate)))
      case VoteStatusRequest =>
        sender() ! VoteStatusResponse(candidate)
    }
  }
  object Citizen {
    final case class Vote(candidate: String)
    case object VoteStatusRequest
    final case class VoteStatusResponse(candidate: Option[String])
  }

  class VoteAggregator extends Actor {
    import Citizen._
    import VoteAggregator._

    override def receive: Receive = onMessage

    def onMessage: Receive = {
      case AggregateVote(citizens) =>
        citizens.foreach(citizen => citizen ! VoteStatusRequest)
        context.become(voted(Map.empty, citizens))
    }

    def voted(rating: Map[String, Int], citizens: Set[ActorRef]): Receive = {
      case VoteStatusResponse(Some(candidate)) =>
        val waitingVoteCitizens = citizens - sender()
        val updatedRating = rating + (candidate -> (rating.getOrElse(candidate, 0) + 1))
        if (waitingVoteCitizens.isEmpty) {
          println(s"Current rating: $rating")
        } else {
          context.become(voted(updatedRating, waitingVoteCitizens))
        }
      case VoteStatusResponse(None) => sender() ! VoteStatusRequest
    }
  }
  object VoteAggregator {
    case object VoteResult
    final case class AggregateVote(citizens: Set[ActorRef])
  }

  import Citizen._
  import VoteAggregator._

  val alice = system.actorOf(Props[Citizen], "alice")
  val bob = system.actorOf(Props[Citizen], "bob")
  val megan = system.actorOf(Props[Citizen], "megan")
  val charlie = system.actorOf(Props[Citizen], "charlie")
  val daniel = system.actorOf(Props[Citizen], "daniel")
  val denis = system.actorOf(Props[Citizen], "denis")
  val anna = system.actorOf(Props[Citizen], "anna")
  val victoria = system.actorOf(Props[Citizen], "victoria")

  alice ! Vote("Martini")
  bob ! Vote("Red Right Hand")
  megan ! Vote("Do Not Sleep")
  charlie ! Vote("Do Not Sleep")
  daniel ! Vote("Red Right Hand")
  denis ! Vote("Martini")
  anna ! Vote("Martini")

  val voteAggregator = system.actorOf(Props[VoteAggregator], "vote-aggregator")

  voteAggregator ! AggregateVote(
    Set(alice, bob, megan, charlie, daniel, denis, anna)
  )
}
