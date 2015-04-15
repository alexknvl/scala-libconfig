name := "scala-libconfig"

version := "0.1-SNAPSHOT"

organization := "com.alexknvl"

scalaVersion := "2.11.6"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies ++= Seq(
    "org.parboiled" %% "parboiled" % "2.1.0",
    "org.scalatest" %% "scalatest" % "2.2.4" % "test"
)
