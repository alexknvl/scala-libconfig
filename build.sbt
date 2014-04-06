name := "scala-libconfig"

version := "0.1-SNAPSHOT"

organization := "com.alexknvl"

scalaVersion := "2.10.3"

scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies ++= Seq(
    "org.parboiled" %% "parboiled" % "2.0-M2",
    "org.scalatest" % "scalatest_2.10" % "2.1.0" % "test"
)
