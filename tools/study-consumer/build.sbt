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
      "io.github.canardlapin" %%% "eyes4s-io"   % "0.0.0-workflow-slices",
      "io.github.canardlapin" %%% "eyes4s-laws" % "0.0.0-workflow-slices" % Test
    )
  )
