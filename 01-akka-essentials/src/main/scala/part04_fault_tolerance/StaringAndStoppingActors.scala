package part04_fault_tolerance

import akka.actor.{
  Actor,
  ActorLogging,
  ActorRef,
  ActorSystem,
  Kill,
  PoisonPill,
  Props,
  Terminated
}

object StaringAndStoppingActors extends App {
  val system = ActorSystem("stopping-actors-demo")

  class Parent extends Actor with ActorLogging {
    import Parent._
    override def receive: Receive = withChildren(Map())

    def withChildren(children: Map[String, ActorRef]): Receive = {
      case StartChild(name) =>
        log.info(s"Starting child with name: $name")
        context.become(
          withChildren(children + (name -> context.actorOf(Props[Child], name)))
        )
      case StopChild(name) =>
        log.info(s"Stop child with name: $name, children: $children")
        children.get(name).foreach(context.stop)
        context.become(withChildren(children - name))
      case Stop =>
        log.info("Stopping myself")
        context.stop(self)
      // It was my solution
      // self ! PoisonPill
      case message =>
        log.info(message.toString)
    }
  }
  object Parent {
    final case class StartChild(name: String)
    final case class StopChild(name: String)
    final case object Stop
  }

  class Child extends Actor with ActorLogging {
    override def receive: Receive = {
      case message => log.info(message.toString)
    }
  }

  import Parent._

  val parent = system.actorOf(Props[Parent], "parent")

  parent ! StartChild("child001")

  val child001 = system.actorSelection("/user/parent/child001")

  child001 ! "Hi, child001!"

  parent ! StopChild("child001")

  // Stop method is async so, this message was accepted and logged
  child001 ! "Goodbye!"

  // But these, not of all, on my laptop 10 was no delivered.
  for (_ <- 1 to 100) child001 ! "Goodbye!"

  parent ! StartChild("child002")

  val child002 = system.actorSelection("/user/parent/child002")

  child002 ! "Hi, there!"

  parent ! "Hi, parent!"

  parent ! Stop

  for (_ <- 1 to 10) parent ! "Parent, are you still there?"
  for (_ <- 1 to 100) child002 ! "Child 002, are you still there, too?"

  // Using special messages:
  val child003 = system.actorOf(Props[Child])
  child003 ! "Hi, child 003!"
  child003 ! PoisonPill
  child003 ! "This message should be in dead letters"

  val child004 = system.actorOf(Props[Child])
  child004 ! "Hi, child 003!"
  child004 ! Kill
  child004 ! "This message should be in dead letters"

  // Death watch
  class Watcher extends Actor with ActorLogging {
    import Parent._

    override def receive: Receive = {
      case StartChild(name) =>
        val child = context.actorOf(Props[Child], name)
        log.info(s"Started and watched child: $name")
        context.watch(child)
      // When watched actor was stopped
      // watched receive special messages Terminated(ActorRef)
      case Terminated(ref) =>
        log.info(s"The reference that I'm watching: $ref has been stopped")
    }
  }

  val watcher = system.actorOf(Props[Watcher], "watcher")
  watcher ! StartChild("child005")
  val child005 = system.actorSelection("/user/watcher/child005")
  child005 ! "Hi, child 005!"
  child005 ! PoisonPill
}
