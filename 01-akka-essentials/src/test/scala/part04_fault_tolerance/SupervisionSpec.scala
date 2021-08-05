package part04_fault_tolerance

import akka.actor.SupervisorStrategy.{Escalate, Restart, Resume, Stop}
import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, AllForOneStrategy, OneForOneStrategy, Props, SupervisorStrategy, Terminated}
import akka.testkit.{EventFilter, ImplicitSender, TestKit}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class SupervisionSpec
    extends TestKit(ActorSystem("supervision-demo"))
    with Matchers
    with ImplicitSender
    with AnyWordSpecLike
    with BeforeAndAfterAll {

  override def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  import SupervisionSpec._

  "A Supervisor" should {
    "resume child in case of a minor fault" in {
      val supervisor = system.actorOf(Props[Supervisor])
      supervisor ! Props[FussyWordCounter]
      val child = expectMsgType[ActorRef]

      child ! "I learn Akka"
      child ! Report
      expectMsg(3)

      child ! "Akka is awesome because I am learning new think in a whole new way"
      child ! Report
      expectMsg(3)
    }

    "restart it's child in case of an empty sentence" in {
      val supervisor = system.actorOf(Props[Supervisor])
      supervisor ! Props[FussyWordCounter]
      val child = expectMsgType[ActorRef]

      child ! "I learn Akka"
      child ! Report
      expectMsg(3)

      child ! ""
      child ! Report
      expectMsg(0)
    }

    "terminate it's child in case of a major error" in {
      val supervisor = system.actorOf(Props[Supervisor])
      supervisor ! Props[FussyWordCounter]
      val child = expectMsgType[ActorRef]

      child ! "I learn Akka"
      child ! Report
      expectMsg(3)

      child ! "hello"
      child ! Report
      expectNoMessage()
    }

    "terminate it's child in case of a major error with watch" in {
      val supervisor = system.actorOf(Props[Supervisor])
      supervisor ! Props[FussyWordCounter]
      val child = expectMsgType[ActorRef]

      watch(child)
      child ! "hello"

      val terminatedMessage = expectMsgType[Terminated]
      terminatedMessage.actor shouldBe child
    }

    "escalate an error when it doesn't know what to do" in {
      val supervisor = system.actorOf(Props[Supervisor], "supervisor")
      supervisor ! Props[FussyWordCounter]
      val child = expectMsgType[ActorRef]

      watch(child)
      child ! 73

      val terminatedMessage = expectMsgType[Terminated]
      terminatedMessage.actor shouldBe child
    }
  }

  "A kinder supervisor" should {
    "not kill children in case it's restarted or escalated failures" in {
      val supervisor = system.actorOf(Props[NoDeathOnRestartSupervisor])
      supervisor ! Props[FussyWordCounter]
      val child = expectMsgType[ActorRef]

      child ! "I learn Akka"
      child ! Report
      expectMsg(3)

      child ! 73
      child ! Report
      expectMsg(0)
    }
  }

  "An all-for-one supervisor" should {
    "apply the all-for-one strategy" in {
      val supervisor = system.actorOf(Props[AllForOneSupervisor])
      supervisor ! Props[FussyWordCounter]
      val child01 = expectMsgType[ActorRef]

      supervisor ! Props[FussyWordCounter]
      val child02 = expectMsgType[ActorRef]

      child02 ! "I learn Akka"
      child02 ! Report
      expectMsg(3)

      EventFilter[NullPointerException]() intercept {
        child01 ! ""
      }

      // Waiting until strategies was applied
      Thread.sleep(750)

      child02 ! Report
      expectMsg(0)
    }
  }
}

object SupervisionSpec {
  class Supervisor extends Actor {
    override val supervisorStrategy: SupervisorStrategy = OneForOneStrategy() {
      case _: NullPointerException     => Restart
      case _: IllegalArgumentException => Stop
      case _: RuntimeException         => Resume
      case _: Exception                => Escalate
    }

    override def receive: Receive = {
      case props: Props =>
        val childRef = context.actorOf(props)
        sender() ! childRef
    }
  }

  class NoDeathOnRestartSupervisor extends Supervisor with ActorLogging {
    override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
      // empty
    }
  }

  class AllForOneSupervisor extends Supervisor {
    override val supervisorStrategy: SupervisorStrategy = AllForOneStrategy() {
      case _: NullPointerException     => Restart
      case _: IllegalArgumentException => Stop
      case _: RuntimeException         => Resume
      case _: Exception                => Escalate
    }
  }

  final case object Report
  class FussyWordCounter extends Actor {
    var words = 0

    override def receive: Receive = {
      case Report => sender() ! words
      case ""     => throw new NullPointerException("Sentence is empty")
      case sentence: String =>
        if (sentence.length > 20)
          throw new RuntimeException("Sentence is too big")
        else if (!Character.isUpperCase(sentence(0)))
          throw new IllegalArgumentException(
            "Sentence must start with uppercase"
          )
        else words += words + sentence.split(" ").length
      case _ => throw new Exception("Can only receive strings")
    }
  }
}
