package part06_paterns

import akka.actor.{Actor, ActorLogging, ActorSystem, Props, Stash}

object StashDemo extends App {
  /*
    ResourceActor:
      - open => it can receives read/write requests to the resource
      - otherwise it will postpone all read/write requests until the state is open

    ResourceActor is close:
      - Open => switch to the open state
      - Read, Write messages are postpone

    ResourceActor is open:
      - Read, Write are handled
      - Close => switch to the close state

    [Open, Read, Read, Write]
      - switch to the open state
      - read the data
      - read the data again
      - write the data

    [Read, Open, Write]
      - stash Read
        Stash: [Read]
      - switch to the open state
      - stash messages prepended to mailbox
        Mailbox: [Read, Write]
      - read and write are handled
   */

  class ResourceActor extends Actor with ActorLogging with Stash {
    import ResourceActor._

    private val storage = Seq.empty[String]

    override def receive: Receive = close(storage)

    def close(storage: Seq[String]): Receive = {
      case Open =>
        log.info("Opening resource...")
        unstashAll()
        context.become(open(storage))
      case message =>
        log.info(s"Stash message: $message")
        stash()
    }

    def open(storage: Seq[String]): Receive = {
      case Read =>
        log.info(s"Read data: $storage")
      case Write(data) =>
        log.info(s"Write new data: $data")
        context.become(open(storage :+ data))
      case Close =>
        log.info("Closing resource...")
        unstashAll()
        context.become(close(storage))
      case message =>
        log.info(s"Stash message: $message")
        stash()
    }
  }
  object ResourceActor {
    case object Open
    case object Close
    case object Read
    case class Write(data: String)
  }

  import ResourceActor._

  val system = ActorSystem("stash-demo")
  val resourceActor = system.actorOf(Props[ResourceActor])

//  resourceActor ! Write("Hello")
//  resourceActor ! Read
//  resourceActor ! Open
//  resourceActor ! Write("darkness")
//  resourceActor ! Read

  resourceActor ! Read
  resourceActor ! Open
  resourceActor ! Open
  resourceActor ! Write("Hello")
  resourceActor ! Close
  resourceActor ! Read
}
