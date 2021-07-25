package part03_testing

import akka.actor.{Actor, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import com.typesafe.config.ConfigFactory
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import scala.util.Random
import scala.concurrent.duration._
import scala.language.postfixOps

class TimedAssertionsSpec
    extends TestKit(ActorSystem("timed-assertions-system", ConfigFactory.load("app.conf")))
    with Matchers
    with ImplicitSender
    with AnyWordSpecLike
    with BeforeAndAfterAll {

  override def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  import TimedAssertionsSpec._

  "A worker actor" should {
    val workerActor = system.actorOf(Props[WorkerActor])

    "reply with 73 in a timely manner" in {
      within(500 millis, 1 second) {
        workerActor ! "work"

        expectMsg(WorkResult(73))
      }
    }

    "reply a valid work at a reasonable cadence" in {
      within(1 second) {
        workerActor ! "workSeq"

        val result: Seq[Int] = receiveWhile[Int](max = 2 seconds, idle = 500 millis, messages = 10) {
          case WorkResult(result) => result
        }

        result.sum shouldBe 10
      }
    }

    "reply to a test probe in a timely manner" in {
      within(1 second) {
        val probe = TestProbe()
        probe.send(workerActor, "work")

        // It's work, because base timeout is 3 seconds
        // But after update default timeout in app.conf it's fail
        // Even we use within block
        probe.expectMsg(WorkResult(73))
      }
    }
  }
}

object TimedAssertionsSpec {
  final case class WorkResult(n: Int)

  class WorkerActor extends Actor {
    override def receive: Receive = {
      case "work" =>
        Thread.sleep(500)
        sender() ! WorkResult(73)
      case "workSeq" =>
        val r = new Random()
        for (_ <- 1 to 10) {
          Thread.sleep(r.nextInt(50))
          sender() ! WorkResult(1)
        }
    }
  }
}
