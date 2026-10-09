import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

ThisBuild / scalaVersion                           := "3.7.4"
ThisBuild / dependencyOverrides += "org.scala-lang" % "scala3-library_3" % scalaVersion.value
ThisBuild / publish / skip                         := true

val library = file("../..").getCanonicalFile

def classpath(module: String) = Seq(
  Compile / unmanagedClasspath ++= IO
    .readLines(library / s"target/js-bundle-sizes/classpaths/$module.txt")
    .filter(_.nonEmpty)
    .map(path => Attributed.blank(file(path)))
)
val common = Seq(
  scalaJSUseMainModuleInitializer := true,
  scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Wunused:all", "-Werror"),
  scalaJSLinkerConfig ~= (_.withSourceMap(false))
)

lazy val codecProbe = project
  .in(file("codec"))
  .enablePlugins(ScalaJSPlugin)
  .settings(common)
  .settings(Compile / mainClass := Some("probe.CodecProbe"))
  .settings(classpath("codec"))

lazy val planProbe = project
  .in(file("plan"))
  .enablePlugins(ScalaJSPlugin)
  .settings(common)
  .settings(Compile / mainClass := Some("probe.PlanProbe"))
  .settings(Compile / unmanagedSourceDirectories += library / "tools/js-bundle-sizes/shared")
  .settings(classpath("plan"))

lazy val fs2Probe = project
  .in(file("fs2"))
  .enablePlugins(ScalaJSPlugin)
  .settings(common)
  .settings(Compile / mainClass := Some("probe.Fs2Probe"))
  .settings(Compile / unmanagedSourceDirectories += library / "tools/js-bundle-sizes/shared")
  .settings(classpath("fs2"))

lazy val probeToolchain = taskKey[Unit]("Record effective compiler and linker versions.")
probeToolchain := {
  IO.write(
    library / "target/js-bundle-sizes/probe-toolchain.properties",
    Seq(
      "codec.scala=" + (codecProbe / scalaVersion).value,
      "plan.scala=" + (planProbe / scalaVersion).value,
      "fs2.scala=" + (fs2Probe / scalaVersion).value,
      "scalaJS=" + scalaJSVersion,
      "sbt=" + sbtVersion.value,
      "sourceMaps=" + (codecProbe / scalaJSLinkerConfig).value.sourceMap
    ).mkString("\n") + "\n"
  )
}
