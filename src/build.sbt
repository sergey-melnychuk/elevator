name := "template"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
//	"com.typesafe.akka" 		%% "akka-actor" 				% "2.3.9",
//	"com.typesafe.akka" 		%% "akka-slf4j" 				% "2.3.9",
	"ch.qos.logback"      	%  "logback-classic"  	% "1.0.0"		% "runtime",
	"org.scalatest" 				%% "scalatest" 					% "2.2.1"		% "test"
)

mainClass in assembly := Some("Main")
