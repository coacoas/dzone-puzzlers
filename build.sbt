scalaVersion := "2.10.3"

name := "dzone"

version := "0.1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.0.RC1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.+" % "test",
  "junit" % "junit" % "4.11" % "test"
)
