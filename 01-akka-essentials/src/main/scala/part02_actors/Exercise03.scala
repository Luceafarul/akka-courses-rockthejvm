package part02_actors

import akka.actor.Actor

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
   * 1, 2, 3, 4, 5 tasks and 7 workers
   * 1, 2, 3, 4, 5, 1, 2
   */

  class WordCounterMaster extends Actor {
    override def receive: Receive = ???
  }
  object WordCounterMaster {
    final case class Initialize(nChildren: Int)
    final case class WordCountTask(/*TODO*/ text: String)
    final case class WordCountReply(/*TODO*/ count: Int)
  }

  class WordCounterWorker extends Actor {
    override def receive: Receive = ???
  }
}
