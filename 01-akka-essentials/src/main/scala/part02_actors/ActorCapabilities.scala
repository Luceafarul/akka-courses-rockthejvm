package part02_actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object ActorCapabilities extends App {
  class SimpleActor extends Actor {
    override def receive: Receive = {
      case message: String if message.startsWith("Hi") =>
        context.sender() ! s"Hello, there, ${sender().path.name}!"
      case message: String =>
        println(s"[${self.path.name}] I have received: $message")
      case number: Int =>
        println(s"[${self.path.name}] I have received number: $number")
      case SpecialMessages(content) =>
        println(
          s"[${self.path.name}] I have received special message with content: $content"
        )
      case SendMessagesToYourself(content) =>
        println(s"[${self.path.name}]... sending this message to myself ...")
        self ! content
      case Info =>
        println(s"Info about: ${context.self} in system: ${context.system}")
      case SayHiTo(ref) =>
        println(s"sending... 'Hi, ${ref.path.name}!' to '$ref'")
        ref ! s"Hi, ${ref.path.name}!"
      case WirelessPhoneMessages(content, ref) => ref forward content + "s"
    }
  }

  val system = ActorSystem("actor-capabilities-demo")
  val simpleActor = system.actorOf(Props[SimpleActor], "simple-actor")

  simpleActor ! "Hello, Actor!"

  // 1. Messages can be of any type
  simpleActor ! 42

  case class SpecialMessages(content: String)
  simpleActor ! SpecialMessages("This is special message")

  // 2. Actors have information about their context and about themselves
  // context.self == this in OOP
  case class SendMessagesToYourself(content: String)
  simpleActor ! SendMessagesToYourself("I'm an actor and I'm proud of it!")

  object Info
  simpleActor ! Info

  // 3. Actors can REPLY to messages
  val alice = system.actorOf(Props[SimpleActor], "alice")
  val bob = system.actorOf(Props[SimpleActor], "bob")

  case class SayHiTo(ref: ActorRef)
  alice ! SayHiTo(bob)

  // Who is the sender in context of this object? When I write simpleActor ! 42
  // This message move to Dead Letters
  // 4. Dead Letters
  alice ! "Hi"

  // 5. Forwarding messages
  // forward message, but keeping original sender
  // D -> A -> B
  // B is get message from D?
  case class WirelessPhoneMessages(content: String, ref: ActorRef)
  alice ! WirelessPhoneMessages(content = "Hi", ref = bob) // sender is noSender, response move to DL

  // def !(message: Any)(implicit sender: ActorRef = Actor.noSender): Unit

  // Default placeholder (null) used for "!" to indicate that there is no sender of the message,
  // that will be translated to the receiving system's deadLetters.
  // final val noSender: ActorRef = null
}
