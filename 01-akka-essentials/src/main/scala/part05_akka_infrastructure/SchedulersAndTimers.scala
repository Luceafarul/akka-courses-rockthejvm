package part05_akka_infrastructure

import akka.actor.{Actor, ActorLogging, ActorSystem, Cancellable, Props, Timers}

import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration._
import scala.language.postfixOps

object SchedulersAndTimers extends App {
  // 1. Scheduler
  class SimpleReminderActor extends Actor with ActorLogging {
    override def receive: Receive = {
      case message => log.info(message.toString)
    }
  }

  val system = ActorSystem("schedulers-timer-demo")

  implicit val executionContext: ExecutionContextExecutor = system.dispatcher

//  val simpleReminderActor = system.actorOf(Props[SimpleReminderActor])
//
//  system.log.info("Scheduling reminder for simpleReminderActor")
//
//  system.scheduler.scheduleOnce(3 seconds) {
//    simpleReminderActor ! "Reminder"
//  }
//
//  val routine: Cancellable =
//    system.scheduler.scheduleWithFixedDelay(2 seconds, 2 seconds) { () =>
//      simpleReminderActor ! "Routine reminder"
//    }
//
//  system.scheduler.scheduleOnce(7 seconds) {
//    routine.cancel()
//  }

  // 2. Timer
  class TimerBasedHeartbeatActor extends Actor with ActorLogging with Timers {
    import TimerBasedHeartbeatActor._

    timers.startSingleTimer(TimerKey, Start, 500 millis )

    override def receive: Receive = {
      case Stop =>
        log.info("Stopping")
        timers.cancel(TimerKey)
        context.stop(self)
      case Start =>
        log.info("Bootstrapping")
        timers.startTimerWithFixedDelay(TimerKey, Reminder, 1 second)
      case Reminder =>
        log.info("I am alive!")
    }
  }
  object TimerBasedHeartbeatActor {
    case object Stop
    case object Start
    case object Reminder
    case object TimerKey
  }

  import TimerBasedHeartbeatActor._

  val timerBasedHeartbeatActor =
    system.actorOf(Props[TimerBasedHeartbeatActor], "timer-actor")

  system.scheduler.scheduleOnce(5 seconds) {
    timerBasedHeartbeatActor ! Stop
  }
}
