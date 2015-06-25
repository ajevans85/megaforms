enablePlugins(ScalaJSPlugin)

lazy val root = crossProject.in(file(".")).settings(
  name := "megaforms",
  version := "1.0",
  organization := "megaforms",
  scalaVersion := "2.11.6",
  libraryDependencies ++= Seq(
    "org.monifu" %%% "minitest" % "0.12" % "test"
  ),
  publishMavenStyle := true,
  testFrameworks += new TestFramework("minitest.runner.Framework"),
  licenses := Seq(("Apache License, Version 2.0", url("http://www.apache.org/licenses/LICENSE-2.0")))
)

lazy val megaformsJVM = root.jvm
lazy val megaformsJS = root.js