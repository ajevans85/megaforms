name := "megaforms"
organization := "megaforms"
version := "1.0-SNAPSHOT"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  "org.monifu" %% "minitest" % "0.12" % "test"
)

testFrameworks += new TestFramework("minitest.runner.Framework")