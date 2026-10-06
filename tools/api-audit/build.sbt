scalaVersion := "3.7.4"
name := "eyes4s-api-audit"
libraryDependencies ++= Seq(
  "org.scala-lang" %% "scala3-tasty-inspector" % scalaVersion.value,
  "org.ow2.asm" % "asm" % "9.9",
  "org.jacoco" % "org.jacoco.core" % "0.8.14",
  "org.jacoco" % "org.jacoco.agent" % "0.8.14" classifier "runtime",
  "io.circe" %% "circe-parser" % "0.14.16",
  "org.scalameta" %% "munit" % "1.3.4" % Test
)
Test / fork := true
Compile / run / fork := true
Compile / run / javaOptions += "-Xmx4g"

lazy val auditAgent = taskKey[Unit]("Write the resolved JaCoCo agent path for the runner")
auditAgent := {
  val agent = (Compile / dependencyClasspath).value.files.find(_.getName.endsWith("-runtime.jar")).get
  IO.write(baseDirectory.value / "target" / "jacoco-agent.txt", agent.getAbsolutePath)
}
