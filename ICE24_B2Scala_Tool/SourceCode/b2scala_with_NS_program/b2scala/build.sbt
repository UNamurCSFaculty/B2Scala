lazy val commonSettings = Seq(
  name := "BachInScala",
  organization := "org.coordinam",
  version := "1.0",
  scalaVersion := "2.12.8"
)

lazy val root = (project in file("."))

scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation")

// set the main class for 'sbt run'
mainClass in (Compile, run) := Some("bscala.bsc_program.BSC_modelling")

// set the main class for packaging the main jar
mainClass in (Compile, packageBin) := Some("BSC_modelling")






