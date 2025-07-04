compile :
	sbt compile

assembly :
	sbt compile
	sbt package
	sbt assembly

run :
	sbt "runMain bscala.bsc_program.BSC_modelling"

run-java :
	java -jar target/scala-2.12/root-assembly-0.1.0-SNAPSHOT.jar
