import sbtcrossproject.CrossPlugin.autoImport._
import scalajscrossproject.ScalaJSCrossPlugin.autoImport._

ThisBuild / scalaVersion := "3.7.4"
// Match the JVM stdlib used by Scala.js macro dependencies to the compiler.
ThisBuild / dependencyOverrides += "org.scala-lang" % "scala3-library_3" % scalaVersion.value

ThisBuild / organization := "example"
ThisBuild / version      := "0.1.0"

lazy val consumer = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("."))
  .settings(
    name := "eyes4s-study-consumer",
    scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Wunused:all", "-Werror"),
    libraryDependencies ++= Seq(
      "io.github.canardlapin" %%% "eyes4s-io"         % "0.0.0-workflow-slices",
      "io.github.canardlapin" %%% "eyes4s-laws"       % "0.0.0-workflow-slices" % Test,
      "org.typelevel"         %%% "munit-cats-effect" % "2.1.0"                 % Test
    )
  )
  .jvmSettings(
    // Optional Arrow transport is an explicit downstream opt-in. Its Jackson
    // dependencies match eyes4s-io's published optional declarations.
    libraryDependencies ++= Seq(
      "org.apache.arrow"               % "arrow-vector"            % "19.0.0" % Test,
      "org.apache.arrow"               % "arrow-memory-unsafe"     % "19.0.0" % Test,
      "com.fasterxml.jackson.core"     % "jackson-core"            % "2.21.7" % Test,
      "com.fasterxml.jackson.core"     % "jackson-databind"        % "2.21.7" % Test,
      "com.fasterxml.jackson.datatype" % "jackson-datatype-jsr310" % "2.21.7" % Test
    ),
    Test / fork := true,
    Test / javaOptions += "--add-opens=java.base/java.nio=ALL-UNNAMED",
    // The response-envelope smoke run times steps and cancellations; suites run
    // one at a time so that no other suite competes with it for processors.
    Test / parallelExecution := false,
    // The fresh readers (JourneyReader for the fixation routes, RouteReader for
    // the recording and temporal routes) are separate JVMs launched over this
    // test classpath: the consumer's own classes and the packaged artifacts.
    // An unforked test cannot read it from java.class.path, so the build
    // writes it as a test resource; verify.py checks every entry.
    Test / resourceGenerators += Def.task {
      val out = (Test / resourceManaged).value / "example" / "fresh-process" / "classpath.txt"
      val entries = (Test / classDirectory).value +: (Test / dependencyClasspath).value.files
      IO.write(out, entries.map(_.getAbsolutePath).mkString(java.io.File.pathSeparator))
      Seq(out)
    }.taskValue
  )
