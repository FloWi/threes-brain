name := "threes-brain"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.1"

libraryDependencies +=  "com.typesafe.akka" % "akka-actor_2.11" % "2.3.3"

libraryDependencies += "log4j" % "log4j" % "1.2.17"

// Akka could be added later
//
//resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
//
//libraryDependencies ++= Seq(
//    "org.scala-lang" % "scala-reflect" % scalaVersion.value,

//    "org.scalatest" % "scalatest_2.11" % "2.1.7" % test
//)
