name := "B2scala_timed_tests"
version := "0.1"
scalaVersion := "2.12.17"  // Use the Scala version compatible with your code

// For testing with Scala 2.12
libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.19"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"

libraryDependencies ++= Seq(
  "org.scalafx" %% "scalafx" % "20.0.0-R31"
)
Compile / run / javaOptions += "-XX:+UseG1GC"
