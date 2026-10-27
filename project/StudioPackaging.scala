/*
 * Copyright 2026 canardlapin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import sbt._
import sbt.Keys._

/** Stage only the runtime classpath; test fixtures and Monocle never enter the app. */
object StudioPackaging {
  val studioPackageInput = taskKey[File]("Stage the self-contained Studio runtime classpath")

  val settings: Seq[Setting[_]] = Seq(
    Compile / mainClass := Some("eyes4s.studio.desktop.StudioMain"),
    studioPackageInput  := {
      val entries = (Runtime / fullClasspath).value.files
      val dest    = (LocalRootProject / target).value / "studio-package" / "input"
      IO.delete(dest)
      IO.createDirectory(dest)
      val jars = entries.zipWithIndex.map { case (source, index) =>
        val name = f"runtime-$index%03d.jar"
        val out  = dest / name
        if (source.isDirectory) {
          val mappings = (source ** "*").get
            .filter(_.isFile)
            .map { f =>
              f -> IO.relativize(source, f).get.replace('\\', '/')
            }
            .sortBy(_._2)
          IO.jar(mappings, out, new java.util.jar.Manifest, Some(0L))
        } else {
          require(source.getName.endsWith(".jar"), s"Non-JAR runtime entry: $source")
          IO.copyFile(source, out)
        }
        name
      }
      val manifest = new java.util.jar.Manifest
      manifest.getMainAttributes.putValue("Manifest-Version", "1.0")
      manifest.getMainAttributes.putValue("Main-Class", "eyes4s.studio.desktop.StudioMain")
      manifest.getMainAttributes.putValue("Class-Path", jars.mkString(" "))
      IO.jar(Seq.empty, dest / "studio-launcher.jar", manifest, Some(0L))
      streams.value.log.info(s"Staged ${jars.size} runtime entries at $dest")
      dest
    }
  )
}
