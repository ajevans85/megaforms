organization := "megaforms"
version := "1.0-SNAPSHOT"
scalaVersion := "2.11.6"

name := "megaforms"
libraryDependencies ++= Seq(
  "org.monifu" %%% "minitest" % "0.12" % "test"
)
testFrameworks += new TestFramework("minitest.runner.Framework")

licenses := Seq(("Apache License, Version 2.0", url("http://www.apache.org/licenses/LICENSE-2.0")))

enablePlugins(ScalaJSPlugin)

lazy val root = crossProject.in(file("."))
lazy val megaformsJVM = root.jvm
lazy val megaformsJS = root.js