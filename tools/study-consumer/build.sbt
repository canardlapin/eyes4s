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
    // The fixation journey's fresh reader is a separate JVM launched over this
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
