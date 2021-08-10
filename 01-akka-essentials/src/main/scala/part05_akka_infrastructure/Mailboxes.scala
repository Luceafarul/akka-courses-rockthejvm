package part05_akka_infrastructure

import akka.actor.{Actor, ActorLogging, ActorSystem, PoisonPill, Props}
import akka.dispatch.{
  ControlMessage,
  PriorityGenerator,
  UnboundedPriorityMailbox
}
import com.typesafe.config.{Config, ConfigFactory}

object Mailboxes extends App {

  val system = ActorSystem(
    "mailbox-demo",
    ConfigFactory.load().getConfig("mailboxes-demo")
  )

  class SimpleActor extends Actor with ActorLogging {
    override def receive: Receive = {
      case message => log.info(s"Receive: $message")
    }
  }

  // Case #1 - custom priority mailbox
  // P0 -> most important
  // P1
  // P2
  // P3

  // Step 1 - mailbox definition
  class SupportTicketPriorityMailbox(
      settings: ActorSystem.Settings,
      config: Config
  ) extends UnboundedPriorityMailbox(
        PriorityGenerator {
          case message: String if message.startsWith("[P0]") => 0
          case message: String if message.startsWith("[P1]") => 1
          case message: String if message.startsWith("[P2]") => 2
          case message: String if message.startsWith("[P3]") => 3
          case _                                             => 4
        }
      )

  // Step 2 - make it known in the config
  // Step 3 - attach the dispatcher to an actor

  val supportTicketLogger = system.actorOf(
    Props[SimpleActor].withDispatcher("support-ticket-dispatcher")
  )

  supportTicketLogger ! PoisonPill
  //  Thread.sleep(1000) // with this messages move to dead-letters
  supportTicketLogger ! "[P3] nice to have"
  supportTicketLogger ! "[P0] should do now"
  supportTicketLogger ! "[P1] if have time"

  // Case #2 - control-aware mailbox

  // Step 1 - mark important messages as control messages
  case object ManagementTicket extends ControlMessage

  // Step 2 - configure who gets the mailbox
  //  - make the actor attach to the mailbox
  val controlAwareActor =
    system.actorOf(Props[SimpleActor].withMailbox("control-mailbox"))

  controlAwareActor ! "[P3] nice to have"
  controlAwareActor ! "[P0] should do now"
  controlAwareActor ! "[P1] if have time"
  controlAwareActor ! ManagementTicket

  val anotherControlAwareActor =
    system.actorOf(Props[SimpleActor], "other-control-aware-actor")

  anotherControlAwareActor ! "[P3] nice to have"
  anotherControlAwareActor ! "[P0] should do now"
  anotherControlAwareActor ! "[P1] if have time"
  anotherControlAwareActor ! ManagementTicket
}
