package part02_actors

import akka.actor.{Actor, ActorSystem, Props}

object ActorsIntro extends App {
  // Part 1 - Actor System
  val actorSystem = ActorSystem("first-actor-system")
  println(actorSystem.name)

  // Part 2 - Create Actor
  class WordCountActor extends Actor {
    // Internal data
    var totalWords = 0

    // Behavior
    // Type alias representing a Receive-expression for Akka Actors.
    // type Receive = PartialFunction[Any, Unit]
    override def receive: Receive = {
      case message: String =>
        println(s"[WordCounter] I have received: $message")
        totalWords += message.split(" ").length
      case msg => println(s"[WordCounter] I cannot understand: ${msg.toString}")
    }
  }

  // Part 3 - Instantiate our actor
  val wordCounter = actorSystem.actorOf(Props[WordCountActor], "word-counter")
  val anotherWordCounter =
    actorSystem.actorOf(Props[WordCountActor], "another-word-counter")

  // Part 4 - Communicate with actor
  wordCounter ! "hello darkness my old friend"
  wordCounter ! 123
  anotherWordCounter ! "send message to another actor..."

  // How to create actor with constructor arguments?
  class Person(name: String) extends Actor {
    override def receive: Receive = {
      case "hi" => println(s"Hi, my name is $name")
      case _    =>
    }
  }

  object Person {
    def props(name: String): Props = Props(new Person(name))
  }

  // This is legal, but best practice is using object with props method.
  val james = actorSystem.actorOf(Props(new Person("James")))
  james ! "hi"

  val freya = actorSystem.actorOf(Person.props("Freya"))
  freya ! "hi"
}
