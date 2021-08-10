package part05_akka_infrastructure

import akka.actor.{Actor, ActorLogging, ActorSystem, Props, Terminated}
import akka.routing.{
  ActorRefRoutee,
  Broadcast,
  FromConfig,
  RoundRobinGroup,
  RoundRobinPool,
  RoundRobinRoutingLogic,
  Router
}
import com.typesafe.config.ConfigFactory

object Routers extends App {
  // 1. Manual router
  class Master extends Actor {
    // Step 1: Create routees (routees based on actors)
    private val workers = for (i <- 1 to 5) yield {
      val worker = context.actorOf(Props[Worker])
      context.watch(worker)
      ActorRefRoutee(worker)
    }

    // Step 2: Define router
    private val router = Router(RoundRobinRoutingLogic(), workers)

    override def receive: Receive = receiveWithRouter(router)

    def receiveWithRouter(router: Router): Receive = {
      // Step 4: Handle termination/lifecycle of the routees messages
      case Terminated(ref) =>
        val worker = context.actorOf(Props[Worker])
        context.watch(worker)
        context.become(
          receiveWithRouter(
            router
              .removeRoutee(ref)
              .addRoutee(worker)
          )
        )
      // Step 3: Route message
      case message => router.route(message, sender)
    }
  }

  class Worker extends Actor with ActorLogging {
    override def receive: Receive = {
      case message => log.info(s"Worker logged message: ${message.toString}")
    }
  }

  val system =
    ActorSystem("router-demo", ConfigFactory.load().getConfig("routers-demo"))
  val master = system.actorOf(Props[Master])

  for (i <- 1 to 10) {
    master ! s"[$i] Hello from the world!"
  }

  // 2. Router actor with its own children - POOL router
  // It can be done with code or from config
  val poolMaster =
    system.actorOf(RoundRobinPool(5).props(Props[Worker]), "pool-master")

  for (i <- 1 to 10) {
    poolMaster ! s"[$i] Hello from the world!"
  }

  val poolMasterFromConfig =
    system.actorOf(FromConfig.props(Props[Worker]), "pool-master-config")

  for (i <- 1 to 10) {
    poolMasterFromConfig ! s"[$i] Hello from the world!"
  }

  // 3. Router with actors created elsewhere - GROUP router
  // Suppose workers was created in another part of app
  val workers = (1 to 5).map(i => system.actorOf(Props[Worker], s"worker_$i"))

  val workerPaths = workers.map(_.path.toString).toList

  val groupMaster = system.actorOf(RoundRobinGroup(workerPaths).props())

  for (i <- 1 to 10) {
    groupMaster ! s"[$i] Hello from the world!"
  }

  val groupMasterFromConfig =
    system.actorOf(FromConfig.props(), "group-master-config")

  for (i <- 1 to 10) {
    groupMasterFromConfig ! s"[$i] Hello from the world!"
  }

  // 4. Special messages
  groupMasterFromConfig ! Broadcast("Hello, from broadcast!")

  // 1. PoisonPill and Kill are NOT routed
  // 2. AddRoutee, RemoveRoutee, GetRoutee handled only by the routing actor
}
