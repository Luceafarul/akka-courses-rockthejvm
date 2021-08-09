package part05_akka_infrastructure

import akka.actor.{Actor, ActorLogging, ActorSystem, Cancellable, Props}

import java.time.LocalTime
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.language.postfixOps

object Exercise01 extends App {
  // Exercise: implement a self-closing actor
  //  - if the actor receives a message (anything), you have 1 second to send it another message
  //  - if the time window expires, the actor will stop itself
  //  - if you send another message, the time window is reset

  class SelfClosingActor extends Actor with ActorLogging {
    override def receive: Receive =
      receiveWithTimeWindow(LocalTime.now())

    def receiveWithTimeWindow(lastMessageTime: LocalTime): Receive = {
      case message =>
        if (LocalTime
              .now()
              .plusSeconds(1L)
              .toSecondOfDay - lastMessageTime.toSecondOfDay > 1) {
          log.info(
            s"Got message after expired time window ($lastMessageTime). Close actor."
          )
          context.stop(self)
        } else {
          log.info(
            s"Get message in time window: ${message.toString}. Next expiration time: $lastMessageTime"
          )
          context.become(
            receiveWithTimeWindow(LocalTime.now().plusSeconds(1L))
          )
        }
    }
  }

  val system = ActorSystem("self-closing-exercise")
  val selfClosingActor =
    system.actorOf(Props[SelfClosingActor], "self-closing-actor")

  implicit val ec: ExecutionContext = system.dispatcher
  val notification =
    system.scheduler.scheduleWithFixedDelay(0.5 seconds, 0.7 seconds) { () =>
      selfClosingActor ! "Notification..."
    }

  system.scheduler.scheduleOnce(5 seconds) {
    notification.cancel()
  }

  system.scheduler.scheduleOnce(10 seconds) {
    selfClosingActor ! "Notification..."
  }

  class SolutionActor extends Actor with ActorLogging {
    import SolutionActor._

    def createTimeoutWindow(): Cancellable =
      context.system.scheduler.scheduleOnce(1 seconds) {
        self ! Timeout
      }

    override def receive: Receive =
      receiveWithTimeoutWindow(createTimeoutWindow())

    def receiveWithTimeoutWindow(schedule: Cancellable): Receive = {
      case Timeout =>
        log.info("Time window expired. Close actor.")
        context.stop(self)
      case message =>
        log.info(s"Receive message: ${message.toString}")
        schedule.cancel()
        context.become(receiveWithTimeoutWindow(createTimeoutWindow()))
    }
  }
  object SolutionActor {
    final case object Timeout
  }

  val solutionActor =
    system.actorOf(Props[SolutionActor], "solution-actor")

  val notificationToSolutionActor =
    system.scheduler.scheduleWithFixedDelay(0.5 seconds, 0.7 seconds) { () =>
      solutionActor ! "Notification..."
    }

  system.scheduler.scheduleOnce(5 seconds) {
    notificationToSolutionActor.cancel()
  }

  system.scheduler.scheduleOnce(10 seconds) {
    solutionActor ! "Notification..."
  }
}
