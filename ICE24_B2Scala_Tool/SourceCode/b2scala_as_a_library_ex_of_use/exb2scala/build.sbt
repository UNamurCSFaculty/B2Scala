lazy val commonSettings = Seq(
  name := "B2Scala Program Example",
  organization := "org.coordinam",
  version := "1.0",
  scalaVersion := "2.12.8"
)

lazy val root = (project in file("."))

scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation")

// set the main class for 'sbt run'
mainClass in (Compile, run) := Some("exbscala.my_program.BSC_modelling")








