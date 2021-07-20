name := "akka-courses-rockthejvm"

version := "0.1"

ThisBuild / scalaVersion := "2.13.6"

val AkkaVersion = "2.6.15"
val ScalaTestVersion = "3.2.8"

lazy val `akka-essentials` = (project in file("01-akka-essentials")).settings(
  libraryDependencies ++= Seq(
    "com.typesafe.akka" %% "akka-actor" % AkkaVersion,
    "com.typesafe.akka" %% "akka-testkit" % AkkaVersion % Test,
    "org.scalatest" %% "scalatest" % ScalaTestVersion % Test
  )
)
