package part05_akka_infrastructure

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import com.typesafe.config.ConfigFactory

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Random

object Dispatchers extends App {
  class Counter extends Actor with ActorLogging {
    override def receive: Receive = counterReceive(1)

    def counterReceive(counter: Int): Receive = {
      case message =>
        log.info(s"[$counter] $message")
        context.become(counterReceive(counter + 1))
    }
  }

  val system = ActorSystem(
    "dispatcher-demo"
    // Uncomment raw below for using with counterActor
    //    ConfigFactory.load().getConfig("dispatchers-demo")
  )

  val actors =
    for (i <- 1 to 10)
      yield system.actorOf(
        Props[Counter].withDispatcher("custom-dispatcher"),
        s"counter-actor-$i"
      )

  val r = new Random()
  for (i <- 1 to 1000) {
    actors(r.nextInt(10)) ! i
  }

  val counterActor = system.actorOf(Props[Counter], "counter-actor")
  for (i <- 1 to 1000) {
    counterActor ! i
  }

  // Dispatchers implement the ExecutionContext trait
  class DBActor extends Actor with ActorLogging {
    // implicit val ec: ExecutionContext = context.dispatcher
    // Solution 1: replace ex
    implicit val ex: ExecutionContext =
      context.system.dispatchers.lookup("custom-dispatcher")
    // Solution 2: use Router

    override def receive: Receive = {
      case message =>
        Future {
          // wait on a resource
          Thread.sleep(5000)
          log.info(s"Success: $message")
        }
    }
  }

  val blockingActor = system.actorOf(Props[DBActor])
  blockingActor ! "Meaning of life is 42"

  val nonBlockingActor = system.actorOf(Props[Counter])
  for (i <- 1 to 1000) {
    val message = s"Important message #$i"
    blockingActor ! message
    nonBlockingActor ! message
  }
}
