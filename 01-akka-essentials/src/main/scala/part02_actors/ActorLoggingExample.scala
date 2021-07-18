package part02_actors

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import akka.event.Logging

object ActorLoggingExample extends App {

  // 1. Explicit logging
  class ActorWithExplicitLogging extends Actor {
    val logger = Logging(context.system, this)

    // 1. DEBUG
    // 2. INFO - default in akka
    // 3. WARNING/WARN
    // 4. ERROR
    override def receive: Receive = {
      case message => logger.info(message.toString)
    }
  }

  val system = ActorSystem("logging-examples")
  val actorWithExplicitLogging = system.actorOf(Props[ActorWithExplicitLogging])

  actorWithExplicitLogging ! "This message should be logged"

  // 2. ActorLogging
  class ActorWithActorLogging extends Actor with ActorLogging {
    override def receive: Receive = {
      case (a, b)  => log.info("Two things: {} and {}", a, b)
      case message => log.info(message.toString)
    }
  }

  val actorWithActorLogging = system.actorOf(Props[ActorWithActorLogging])

  actorWithActorLogging ! "This message should be logged too"
  actorWithActorLogging ! (42, 73)
}
