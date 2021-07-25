package part03_testing

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.wordspec.AnyWordSpecLike

class TestProbSpec
    extends TestKit(ActorSystem("test-probe-system"))
    with ImplicitSender
    with AnyWordSpecLike
    with BeforeAndAfterAll {

  override protected def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  import TestProbSpec._

  "A master actor" should {
    "register a worker" in {
      val master = system.actorOf(Props[Master])
      val worker = TestProbe("worker")

      master ! Register(worker.ref)

      expectMsg(Registered)
    }

    "send the work to the worker actor" in {
      val master = system.actorOf(Props[Master])
      val worker = TestProbe("worker")
      val text = "Hello darkness my old friend"

      master ! Register(worker.ref)
      expectMsg(Registered)

      master ! Work(text)

      // testActor because it's implicit sender
      // the interaction between the master and the worker
      worker.expectMsg(WorkerTask(text, testActor))
      worker.reply(TaskCompleted(5, testActor))

      expectMsg(Report(5))
    }

    "aggregate data correctly" in {
      val master = system.actorOf(Props[Master])
      val worker = TestProbe("worker")
      val text = "Hello darkness my old friend"

      master ! Register(worker.ref)
      expectMsg(Registered)

      master ! Work(text)
      master ! Work(text)

      // In the meantime we don't have a worker actor, we implement only master
      // So... we create test behavior implementations
      worker.receiveWhile() {
        case WorkerTask(`text`, `testActor`) => worker.reply(TaskCompleted(5, testActor))
      }

      expectMsg(Report(5))
      expectMsg(Report(10))
    }
  }
}

object TestProbSpec {

  /**
    * Scenario:
    * Word counting actor hierarchy master-worker
    *
    * Send some work to the master
    *  - master sends the worker piece of work
    *  - worker processes the work and replies to master
    *  - master aggregates the result
    * Master sends the total count to the original requester
    */
  final case class Work(text: String)
  final case class WorkerTask(text: String, requester: ActorRef)
  final case class TaskCompleted(wordsCount: Int, requester: ActorRef)
  final case class Register(workerRef: ActorRef)
  final case class Report(wordsCount: Int)
  final case object Registered

  class Master extends Actor {
    override def receive: Receive = {
      case Register(workerRef) =>
        sender() ! Registered
        context.become(readyToWork(workerRef, 0))
      case _ => // Ignore other messages
    }

    def readyToWork(workerRef: ActorRef, totalWordsCount: Int): Receive = {
      case Work(text) =>
        workerRef ! WorkerTask(text, sender())
      case TaskCompleted(wordsCount, requester) =>
        val newTotalWordsCount = totalWordsCount + wordsCount
        requester ! Report(newTotalWordsCount)
        context.become(readyToWork(workerRef, newTotalWordsCount))
    }
  }
}
