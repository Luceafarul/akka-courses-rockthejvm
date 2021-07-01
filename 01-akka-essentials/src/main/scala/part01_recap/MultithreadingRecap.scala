package part01_recap

import scala.concurrent.Future
import scala.util.{Failure, Success}

object MultithreadingRecap extends App {
  // Creating threads on the JVM
  val aThread = new Thread(new Runnable {
    override def run(): Unit = println("I'm running in parallel.")
  })
  val aThreadToo = new Thread(() => println("I'm running in parallel too."))

  aThread.start()
  aThread.join()

  aThreadToo.start()
  aThreadToo.join()

  // Different runs produce different results
  val threadHello = new Thread(() => (1 to 1000).foreach(_ => println("hello...")))
  val threadGoodbye = new Thread(() => (1 to 1000).foreach(_ => println("goodbye...")))
  threadHello.start()
  threadGoodbye.start()

  /*
   * BA(10000)
   *
   * T1 -> withdraw 1000
   * T2 -> withdraw 2000
   *
   * T1 -> this.amount = amount - .... // PREEMPTED by the OS
   * T2 -> this.amount = amount - 2000 = 8000
   * T1 -> -1000 = 9000
   *
   * => result = 9000
   *
   * this.amount = this.amount - 1000 is NOT ATOMIC
   */
  case class BankAccount(private var amount: Int) {
    def withdraw(amount: Int): Unit = this.amount -= amount

    def safeWithdraw(amount: Int): Unit = this.synchronized {
      this.amount -= amount
    }

    override def toString: String = amount.toString
  }

  // Inter-thread communication on the JVM
  // Wait and notify mechanism

  // Scala Futures

  import scala.concurrent.ExecutionContext.Implicits.global

  val future = Future {
    // ... some long computation on different threads
    42
  }

  future.onComplete {
    case Success(value) =>
      println(s"I found the meaning of life. It is the $value")
    case Failure(exception) =>
      println(s"Something happened with the meaning of life: ${exception.getMessage}")
  }

  printFutureOnComplete(future.map(_ + 1))
  printFutureOnComplete(future.flatMap(value => Future(value + 2)))
  printFutureOnComplete(future.filter(_ % 10 == 0))

  for {
    value <- future
  } yield println(s"Result of for-comprehension is $value")

  private def printFutureOnComplete[A](f: Future[A]): Unit = f.onComplete {
    case Success(value) => println(s"Future value is $value")
    case Failure(exception) => println(s"Future error: ${exception.getMessage}")
  }
}
