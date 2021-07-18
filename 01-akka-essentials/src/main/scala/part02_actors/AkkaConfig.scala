package part02_actors

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import com.typesafe.config.ConfigFactory

import java.io.File

object AkkaConfig extends App {
  // 1. Inline configuration
  val configString =
    """
      |akka {
      |  loglevel = DEBUG
      |}
      |""".stripMargin

  class SimpleLoggedActor extends Actor with ActorLogging {
    override def receive: Receive = {
      case message => log.info(message.toString)
    }
  }

  val config = ConfigFactory.parseString(configString)
  val system = ActorSystem("system-config-example", ConfigFactory.load(config))
  val simpleLoggedActor01 = system.actorOf(Props[SimpleLoggedActor])

  simpleLoggedActor01 ! "Just message"

  // 2. Config file (application.conf under resources dir)
  val systemWithConfigFile = ActorSystem("system-config-file-example")
  val simpleLoggedActor02 = systemWithConfigFile.actorOf(Props[SimpleLoggedActor])

  simpleLoggedActor02 ! "Just second message"

  // 3. Separate config in same file
  val specialConfig = ConfigFactory.load().getConfig("customConfig")
  val specialConfigSystem = ActorSystem("system-special-config-example", specialConfig)
  val simpleLoggedActor03 = specialConfigSystem.actorOf(Props[SimpleLoggedActor])
  val message03 = specialConfig.getString("akka.message")

  simpleLoggedActor03 ! message03//"Third message"

  // 4. Separate config in another file
  val separateFileConfig = ConfigFactory.load("config/app.conf")
  val separateFileConfigSystem = ActorSystem("system-special-config-example", separateFileConfig)
  val simpleLoggedActor04 = specialConfigSystem.actorOf(Props[SimpleLoggedActor])
  val message04 = separateFileConfig.getString("akka.message")

  simpleLoggedActor04 ! message04

  // 5. Different file formats (JSON, Properties)
  val jsonFileConfig = ConfigFactory.load("json/app.json")
  val jsonFileConfigSystem = ActorSystem("system-special-config-example", jsonFileConfig)
  val simpleLoggedActor05 = specialConfigSystem.actorOf(Props[SimpleLoggedActor])
  val message05 = jsonFileConfig.getString("message")

  simpleLoggedActor05 ! message05
}
