package part02_actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

import scala.collection.immutable.Queue

object Exercise03 extends App {
  // Distributed Word Counter

  /*
   * Create WordCounterMaster
   *  - send Initialize(10) to WordCounterMaster
   *  - send "Some text to count word" to WordCounterMaster
   *     - WordCounterMaster will send a WordCountTask("...") to one of it's children
   *        - Child replies with a WordCountReply(word count) to the WordCounterMaster
   *     - WordCounterMaster replies with "word count" to the sender
   *
   * Requester -> WordCounterMaster -> WordCounterWorker
   *           <- WordCounterMaster <-
   *
   * Use round robin logic:
   * 1, 2, 3, 4, 5 workers and 7 tasks
   * 1, 2, 3, 4, 5, 1, 2
   */

  class WordCounterMaster extends Actor {
    import WordCounterMaster._

    override def receive: Receive = {
      case Initialize(n) =>
        val workers =
          for (i <- 1 to n)
            yield context.actorOf(Props[WordCounterWorker], s"worker-$i")
        context.become(workersReady(workers.toList, List.empty))
    }

    def workersReady(workers: List[ActorRef], tasks: List[String]): Receive = {
      case message: String =>
        if (workers.isEmpty) {
          println("Waiting for available worker...")
          context.become(workersReady(workers, tasks :+ message))
        } else {
          workers.head ! WordCountTask(message)
          context.become(workersReady(workers.tail, tasks))
        }
      case WordCountReply(workerRef, wordCount) =>
        println(s"Word count in ... $wordCount")
        if (tasks.nonEmpty && workers.nonEmpty) {
          workerRef ! WordCountTask(tasks.head)
          context.become(workersReady(workers, tasks.tail))
        } else {
          context.become(workersReady(workers :+ workerRef, tasks))
        }
    }
  }
  object WordCounterMaster {
    final case class Initialize(nChildren: Int)
    final case class WordCountTask(text: String)
    final case class WordCountReply(workerRef: ActorRef, count: Int)
  }

  class WordCounterWorker extends Actor {
    import WordCounterMaster._

    override def receive: Receive = {
      case WordCountTask(text) =>
        if (text.isEmpty) sender() ! WordCountReply(self, 0)
        else {
          println(s"${self.path.name}: counting words...")
          val wordCount = text.split(" ").length
          sender() ! WordCountReply(self, wordCount)
        }
    }
  }

  val system = ActorSystem("distributed-word-counter")

  import WordCounterMaster._
  val wordCounterMaster =
    system.actorOf(Props[WordCounterMaster], "word-counter-master")

  wordCounterMaster ! Initialize(2)
  wordCounterMaster ! "Hello darkness my old friend"
  wordCounterMaster ! "Hello how are you?"
  wordCounterMaster ! "I learn Akka! And I listen music when learn."
  wordCounterMaster ! "I learn Scala!"
}
