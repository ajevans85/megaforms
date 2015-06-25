enablePlugins(ScalaJSPlugin)

lazy val megaforms = crossProject
  .settings(
    version := "1.0",
    name := "megaforms",
    organization := "megaforms",
    scalaVersion := "2.11.6",

    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomIncludeRepository := { _ => false },


    licenses := Seq(("Apache License, Version 2.0", url("http://www.apache.org/licenses/LICENSE-2.0")))
  ).jsSettings(
    libraryDependencies ++= Seq(
      "org.monifu" %% "minitest" % "0.12" % "test"
    ),
    testFrameworks += new TestFramework("minitest.runner.Framework")
  ).jvmSettings(
    libraryDependencies ++= Seq(
      "org.monifu" %%% "minitest" % "0.12" % "test"
    ),
    testFrameworks += new TestFramework("minitest.runner.Framework")
  )

lazy val megaformsJVM = megaforms.jvm
lazy val megaformsJS = megaforms.js

lazy val root = preventPublication(project.in(file(".")))
  .settings()
  .aggregate(megaformsJS, megaformsJVM)

def preventPublication(p: Project) =
  p.settings(
    publish :=(),
    publishLocal :=(),
    publishArtifact := false,
    publishTo := Some(Resolver.file("Unused transient repository", target.value / "fakepublish")),
    packagedArtifacts := Map.empty)