name := "megaforms root"

organization in ThisBuild := "megaforms"
version in ThisBuild := "1.0-SNAPSHOT"
scalaVersion in ThisBuild := "2.11.6"

licenses in ThisBuild := Seq(("Apache License, Version 2.0", url("http://www.apache.org/licenses/LICENSE-2.0")))


enablePlugins(ScalaJSPlugin)

lazy val root = project.in(file("."))
  .aggregate(megaformsJVM, megaformsJS)
  .settings(
    publish := {},
    publishLocal := {},
    libraryDependencies ++= Seq(
      "org.monifu" %%% "minitest" % "0.12" % "test"
    ),
    testFrameworks += new TestFramework("minitest.runner.Framework")
  )

lazy val megaforms = crossProject.in(file(".")).
  settings(
    libraryDependencies ++= Seq(
      "org.monifu" %%% "minitest" % "0.12" % "test"
    )
  )
  .jvmSettings()
  .jsSettings()

lazy val megaformsJVM = megaforms.jvm
lazy val megaformsJS = megaforms.js