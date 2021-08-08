package part04_fault_tolerance

import akka.actor.SupervisorStrategy.Stop
import akka.actor.{Actor, ActorLogging, ActorSystem, OneForOneStrategy, Props}
import akka.pattern.{BackoffOpts, BackoffSupervisor}

import java.io.File
import scala.concurrent.duration._
import scala.io.Source
import scala.language.postfixOps

object BackoffSupervisorPattern extends App {

  case object ReadFile
  class FileBasedPersistentActor extends Actor with ActorLogging {
    var dataSource: Source = null

    override def preStart(): Unit =
      log.info("Persistent actor starting...")

    override def postStop(): Unit =
      log.info("Persistent actor stopped...")

    override def preRestart(reason: Throwable, message: Option[Any]): Unit =
      log.info(
        s"Persistent actor restarting. With reason: ${reason.getMessage}"
      )

    override def receive: Receive = {
      case ReadFile =>
        if (dataSource == null)
          dataSource = Source.fromFile(
            new File(
              "src/main/resources/backoff/persistent-actor.txt"
            )
          )
        log.info(
          s"I've just read some IMPORTANT data: ${dataSource.getLines().toList}"
        )
    }
  }

  val system = ActorSystem("backoff-supervisor-demo")
//  val simpleActor =
//    system.actorOf(Props[FileBasedPersistentActor], "simple-actor")
//  simpleActor ! ReadFile

  val simpleBackoffSupervisorProps = BackoffSupervisor.props(
    BackoffOpts.onFailure(
      Props[FileBasedPersistentActor],
      "simple-backoff-actor",
      3 seconds,
      30 seconds,
      0.2
    )
  )

  // Simple Supervisor:
  //  - child called simple-backoff-actor (props of type FileBasedPersistentActor]
  //  - supervision strategy is the default one (restarting on everything)
  //    - first attempt after 3 seconds
  //    - next attempt is 2x the previous attempt - 6s, 12s, 24s
//  val simpleBackoffSupervisor =
//    system.actorOf(simpleBackoffSupervisorProps, "simple-supervisor")
//  simpleBackoffSupervisor ! ReadFile

  val stopBackoffSupervisorProps = BackoffSupervisor.props(
    BackoffOpts
      .onStop(
        Props[FileBasedPersistentActor],
        "stop-backoff-actor",
        3 seconds,
        30 seconds,
        0.2
      )
      .withSupervisorStrategy(
        OneForOneStrategy() {
          case _ => Stop
        }
      )
  )

//  val stopBackoffSupervisor =
//    system.actorOf(stopBackoffSupervisorProps, "stop-supervisor")
//  stopBackoffSupervisor ! ReadFile

  class EagerFileBasedPersistentActor extends FileBasedPersistentActor {
    override def preStart(): Unit = {
      log.info("Eager actor starting...")
      dataSource = Source.fromFile(
        new File(
          "src/main/resources/backoff/persistent-actor.txt"
        )
      )
    }
  }

//  val eagerActor =
//    system.actorOf(Props[EagerFileBasedPersistentActor], "eager-actor")
//  eagerActor ! ReadFile // Default behavior if get ActorInitializationException => STOP

  val repeatedBackoffSupervisorProps = BackoffSupervisor.props(
    BackoffOpts.onStop(
      Props[EagerFileBasedPersistentActor],
      "eager-backoff-actor",
      1 seconds,
      30 seconds,
      0.1
    )
  )

  // Eager Supervisor:
  //  - child eager-actor
  //    - will die on start with ActorInitializationException
  //    - trigger the supervision strategy in repeated-supervisor => stop eager-backoff-actor
  //  - backoff will kick in after 1 seconds, then 2 seconds, 4s, 8s, 16s

  val repeatedBackoffSupervisor =
    system.actorOf(repeatedBackoffSupervisorProps, "repeated-supervisor")
  repeatedBackoffSupervisor ! ReadFile
}
