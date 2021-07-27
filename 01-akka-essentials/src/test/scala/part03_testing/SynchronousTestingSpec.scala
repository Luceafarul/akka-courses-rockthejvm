package part03_testing

import akka.actor.{Actor, ActorSystem, Props}
import akka.testkit.{CallingThreadDispatcher, TestActorRef, TestProbe}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import scala.concurrent.duration.Duration

class SynchronousTestingSpec
    extends AnyWordSpecLike
    with Matchers
    with BeforeAndAfterAll {

  implicit val system = ActorSystem("synchronous-testing-spec")

  override def afterAll(): Unit = system.terminate()

  /**
   * All of these are same tests
   * that check state of actor in synchronous way
   * but using different approach for this
   *
   * Synchronous tests: all messages are handled in the calling thread
   * Option 1: TestActorRef
   * Option 2: CallingThreadDispatcher
   */

  "A Counter actor" should {
    import SynchronousTestingSpec._

    "synchronously increment counter" in {
      val counter = TestActorRef[Counter](Props[Counter])

      counter ! Inc // Counter has ALREADY receive the message

      counter.underlyingActor.counter shouldBe 1
    }

    "synchronously increment counter at the call of receive function" in {
      val counter = TestActorRef[Counter](Props[Counter])

      counter.receive(Inc)

      counter.underlyingActor.counter shouldBe 1
    }

    "work on the calling thread dispatcher" in {
      val counter = system.actorOf(
        Props[Counter].withDispatcher(CallingThreadDispatcher.Id)
      )
      val probe = TestProbe()

      probe.send(counter, Read) // probe hsa ALREADY received the message 0
      probe.expectMsg(
        Duration.Zero,
        0
      ) // If we remove .withDispatcher above, assertion failed
    }
  }
}

object SynchronousTestingSpec {
  final case object Inc
  final case object Read

  class Counter extends Actor {
    var counter = 0

    override def receive: Receive = inc(0)

    def inc(counter: Int): Receive = {
      case Inc =>
        this.counter = counter + 1
        context.become(inc(counter + 1))
      case Read => sender() ! counter
    }
  }
}
