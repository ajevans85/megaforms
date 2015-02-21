name := "megaforms"
organization := "megaforms"
version := "1.0-SNAPSHOT"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  "org.monifu" %% "minitest" % "0.12" % "test"
)

licenses := Seq(("Apache License, Version 2.0", url("http://www.apache.org/licenses/LICENSE-2.0")))


testFrameworks += new TestFramework("minitest.runner.Framework")