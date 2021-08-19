package part06_patterns

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.pattern.pipe
import akka.testkit.{ImplicitSender, TestKit}
import akka.util.Timeout
import org.scalatest.BeforeAndAfterAll
import org.scalatest.wordspec.AnyWordSpecLike

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.{Failure, Success}

class AskSpec
    extends TestKit(ActorSystem("ask-spec"))
    with ImplicitSender
    with AnyWordSpecLike
    with BeforeAndAfterAll {

  override def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  import AskSpec._
  import AskSpec.AuthManager._
  import AskSpec.AuthManager.AuthFailure._

  "An authenticator" should {
    authenticatorTestSuite(Props[AuthManager])
  }

  "An piped authenticator" should {
    authenticatorTestSuite(Props[PipeAuthManager])
  }

  def authenticatorTestSuite(props: Props): Unit = {
    "fail to authenticate a non-register user" in {
      val authManager = system.actorOf(props)
      authManager ! Authenticate("yaroslav", "12345")
      expectMsg(UserNotFound("Username: yaroslav not found"))
    }

    "fail to authenticate with invalid password" in {
      val authManager = system.actorOf(props)
      authManager ! Register("yaroslav", "12345")
      authManager ! Authenticate("yaroslav", "123")
      expectMsg(IncorrectPassword())
    }

    "successfully authenticate register user" in {
      val authManager = system.actorOf(props)
      authManager ! Register("yaroslav", "12345")
      authManager ! Authenticate("yaroslav", "12345")
      expectMsg(AuthSuccess)
    }
  }
}

object AskSpec {
  // Assume this code somewhere else in your application
  class KeyValueActor extends Actor with ActorLogging {
    import KeyValueActor._

    override def receive: Receive = online(Map.empty)

    def online(store: Map[String, String]): Receive = {
      case Read(key) =>
        log.info(s"Trying to read value for key: $key")
        sender() ! store.get(key)
      case Write(key, value) =>
        log.info(s"Writing the value: $value for key: $key")
        context.become(online(store + (key -> value)))
    }
  }
  object KeyValueActor {
    final case class Read(key: String)
    final case class Write(key: String, value: String)
  }

  // User authenticator actor
  class AuthManager extends Actor with ActorLogging {
    import AuthManager._
    import AuthManager.AuthFailure._
    import KeyValueActor._

    implicit val timeout: Timeout = Timeout(1 second)
    implicit val ec: ExecutionContext = context.dispatcher

    protected val authDb: ActorRef = context.actorOf(Props[KeyValueActor])

    override def receive: Receive = {
      case Register(username, password) =>
        log.info(s"Try to register user: $username")
        authDb ! Write(username, password)
      case Authenticate(username, password) =>
        log.info(s"Try to authenticate user: $username")
        handleAuthentication(username, password)
    }

    def handleAuthentication(username: String, password: String): Unit = {
      val originalSender = sender()
      val futurePassword = authDb ? Read(username)
      // future handling? what if actor is dead? and we never get response?
      // sender() returns last sender of the message - when future finished sender was different

      // NEVER CALL METHODS ON THE ACTOR INSTANCE
      // OR ACCESS MUTABLE STATE IN onComplete

      // Avoid closing over the actor instance or mutable state
      futurePassword.onComplete {
        case Success(None) =>
          log.info(s"Failed to authenticate user: $username. User not found.")
          originalSender ! UserNotFound(s"Username: $username not found")
        case Success(Some(value)) =>
          log.info(s"Failed to authenticate user: $username. User not found.")
          if (value == password) originalSender ! AuthSuccess
          else originalSender ! IncorrectPassword()
        case Failure(exception) =>
          val message = exception.getMessage
          log.info(s"Failed to authenticate user: $username, reason: $message")
          originalSender ! GeneralAuthFailure(s"Failed with: $message")
      }
    }
  }
  object AuthManager {
    final case class Register(username: String, password: String)
    final case class Authenticate(username: String, password: String)
    final case object AuthSuccess

    object AuthFailure {
      case class GeneralAuthFailure(message: String)
      case class UserNotFound(message: String)
      case class IncorrectPassword(message: String = "Incorrect password")
    }
  }

  class PipeAuthManager extends AuthManager {
    import KeyValueActor._
    import AuthManager._
    import AuthManager.AuthFailure._

    override def handleAuthentication(
        username: String,
        password: String
    ): Unit = {
      val future: Future[Any] = authDb ? Read(username)
      val passwordFuture = future.mapTo[Option[String]]
      val responseFuture = passwordFuture.map {
        case Some(value) =>
          log.info(s"Failed to authenticate user: $username. User not found.")
          if (value == password) AuthSuccess else IncorrectPassword()
        case None =>
          log.info(s"Failed to authenticate user: $username. User not found.")
          UserNotFound(s"Username: $username not found")
      }

      responseFuture.pipeTo(sender())
    }
  }
}
