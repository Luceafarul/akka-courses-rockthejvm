package part04_fault_tolerance

import akka.actor.{Actor, ActorLogging, ActorSystem, PoisonPill, Props}

object ActorLifecycle extends App {

  // preStart and postStop
  final case object StartChild
  class LifecycleActor extends Actor with ActorLogging {

    override def preStart(): Unit = log.info("I'm starting...")

    override def postStop(): Unit = log.info("I'm stopped")

    override def receive: Receive = {
      case StartChild =>
        context.actorOf(Props[LifecycleActor], "child")
    }
  }

  val system = ActorSystem("lifecycle-demo-system")
  val parent = system.actorOf(Props[LifecycleActor], "parent")

//  parent ! StartChild
//  parent ! PoisonPill

  // restart
  final case object Fail
  final case object FailChild
  final case object Check
  final case object CheckChild
  class Parent extends Actor with ActorLogging {
    val child = context.actorOf(Props[Child], "supervised-child")

    override def receive: Receive = {
      case FailChild  => child ! Fail
      case CheckChild => child ! Check
    }
  }

  class Child extends Actor with ActorLogging {
    override def preStart(): Unit = log.info("I'm starting...")
    override def postStop(): Unit = log.info("I'm stopped")

    override def preRestart(reason: Throwable, message: Option[Any]): Unit =
      log.info(
        s"Supervised actor restarting because of: ${reason.getMessage} after get: $message"
      )

    override def postRestart(reason: Throwable): Unit =
      log.info(s"Supervised actor restarted after: ${reason.getMessage}")

    override def receive: Receive = {
      case Fail =>
        log.warning("child fail now...")
        throw new RuntimeException("I'm failed... :(")
      case Check =>
        log.info("Checking child. I'm OK.")
    }
  }

  val supervisor = system.actorOf(Props[Parent], "supervisor")
  supervisor ! FailChild
  supervisor ! CheckChild
}
