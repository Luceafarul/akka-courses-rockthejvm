package part03_testing

import akka.actor.{Actor, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.Random

class IntroductionSpec
    extends TestKit(ActorSystem("introduction-testkit-system"))
    with ImplicitSender
    with Matchers
    with AnyWordSpecLike
    with BeforeAndAfterAll {

  override protected def afterAll(): Unit =
    shutdown(system)

  import IntroductionSpec._

  "An echo actor" should {
    "send back the same message" in {
      val echoActor = system.actorOf(Props[EchoActor])
      val message = "Hello, testkit!"

      echoActor ! message

      expectMsg(message)
    }
  }

  "A black hole actor" should {
    "do not reply to the sender" in {
      val blackHoleActor = system.actorOf(Props[Blackhole])
      val message = "Hello, testkit!"

      blackHoleActor ! message

      expectNoMessage(1 seconds)
    }
  }

  "A lab test actor" should {
    val labTestActor = system.actorOf(Props[LabTestActor])

    "turn string into uppercase" in {
      labTestActor ! "akka"

      val result = expectMsgType[String]

      result shouldBe "AKKA"
    }

    "reply to a greeting message" in {
      labTestActor ! "greeting"

      expectMsgAnyOf("hi", "hello")
    }

    "reply with a favorite tech" in {
      labTestActor ! "tech"

      expectMsgAllOf("Scala", "Akka")
    }

    "reply with a favorite tech in a different way" in {
      labTestActor ! "tech"

      receiveN(2) shouldBe Seq("Scala", "Akka")
    }

    "reply with a favorite tech in a fancy way" in {
      labTestActor ! "tech"

      expectMsgPF() {
        case "Scala" =>
        case "Akka"  => // for more granular checking
      }
    }
  }
}

object IntroductionSpec {
  class EchoActor extends Actor {
    override def receive: Receive = {
      case message => sender() ! message
    }
  }

  class Blackhole extends Actor {
    override def receive: Receive = Actor.emptyBehavior
  }

  class LabTestActor extends Actor {
    val random = new Random()

    override def receive: Receive = {
      case "greeting" =>
        if (random.nextBoolean()) sender() ! "hi" else sender() ! "hello"
      case "tech" =>
        sender() ! "Scala"
        sender() ! "Akka"
      case message: String => sender() ! message.toUpperCase
    }
  }
}
