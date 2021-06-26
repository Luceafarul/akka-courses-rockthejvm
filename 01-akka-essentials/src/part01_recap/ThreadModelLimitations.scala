package part01_recap

import scala.concurrent.Future

object ThreadModelLimitations extends App {
  /**
   * Daniel's rants:
   * DR #1: OOP encapsulation is only valid in the SINGLE THREADED MODEL.
   */
  case class BankAccount(private var amount: Int) {
    def deposit(amount: Int): Unit = this.synchronized {
      this.amount += amount
    }
    def withdraw(amount: Int): Unit = this.synchronized {
      this.amount -= amount
    }
    def balance: Int = amount
  }

  val account = BankAccount(2000)
  for (_ <- 1 to 1000) new Thread(() => account.withdraw(1)).start()
  for (_ <- 1 to 1000) new Thread(() => account.deposit(1)).start()

  println(account.balance)

  // OOP encapsulation is broken in a multithreaded env
  // Synchronization! Locks to the rescue!
  // Deadlocks, livelocks

  // We need a data structure with:
  // - fully encapsulated
  // - with no locks

  /**
   * DR #2: Delegating something to a thread is a PAIN
   */

  // If you have a running thread and you want to pass a runnable to that thread. How to do it?

  var task: Runnable = null
  val thread: Thread = new Thread(() => {
    while (true) {
      while (task == null) {
        thread.synchronized {
          println("[background] waiting for a task...")
          thread.wait()
        }
      }

      task.synchronized {
        println("[background] I have a task!")
        task.run()
        task = null
      }
    }
  })

  def delegateToBackgroundThread(r: Runnable): Unit = {
    if (task == null) task = r
    thread.synchronized {
      thread.notify()
    }
  }

  thread.start()
  Thread.sleep(1000)
  delegateToBackgroundThread(() => println("Hello!"))
  delegateToBackgroundThread(() => println(42))
  Thread.sleep(1000)
  delegateToBackgroundThread(() => println("This should run in the background!"))

  // What should we do with:
  // - Other signals?
  // - Multiple background tasks and threads?
  // - Who gave the signal?
  // - What if I crash?

  // Need a data structure which:
  // - can safely receive messages
  // - can identify the sender
  // - is easily identifiable
  // - can guard against errors

  /**
   * DR #3: Tracing and dealing with errors in multithreaded env is a PAIN
   */

  // 1M numbers is between 10 threads
  import scala.concurrent.ExecutionContext.Implicits.global
  val future = (0 to 9)
    .map(i => 1_000_000 * i until 1_000_000 * (i + 1))
    .map { range => Future {
      if (range.contains(834352)) throw new RuntimeException("Invalid number")
      range.sum
    }}

  val sumFuture = Future.reduceLeft(future)(_ + _) // Future with the sum of all the numbers

  sumFuture.onComplete(f => println(s"Result of sum: $f"))
}
