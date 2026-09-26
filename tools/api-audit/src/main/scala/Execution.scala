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

package eyes4s.audittool

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import io.circe.Json
import org.objectweb.asm.*
import org.jacoco.core.analysis.{Analyzer, CoverageBuilder}
import org.jacoco.core.tools.ExecFileLoader

/** Method probes are attributed to a single forked suite, including its fixture setup. */
object Execution:
  def main(args: Array[String]): Unit =
    require(args.length == 2, "Execution ROOT OUTPUT")
    val root = Path.of(args(0)).toAbsolutePath.normalize
    val modules = Vector("kernel", "core", "detect", "surface", "aoi", "compare", "design", "plan", "results", "codec", "laws", "fs2", "io")
    val classes = scala.collection.mutable.Map.empty[String, (Path, String)]
    val declarations = scala.collection.mutable.ArrayBuffer.empty[Json]
    for module <- modules do
      val stream = Files.walk(root.resolve(s"$module/.jvm/target/scala-3.7.4/classes"))
      try stream.iterator.asScala.filter(_.toString.endsWith(".class")).foreach { path =>
        val reader = new ClassReader(Files.readAllBytes(path))
        classes(reader.getClassName) = (path, module)
        val methods = scala.collection.mutable.ArrayBuffer.empty[Json]
        var source = ""
        reader.accept(new ClassVisitor(Opcodes.ASM9):
          override def visitSource(name: String, debug: String): Unit = source = name
          override def visitMethod(access: Int, name: String, descriptor: String, signature: String, exceptions: Array[String]): MethodVisitor =
            new MethodVisitor(Opcodes.ASM9):
              val lines = scala.collection.mutable.ArrayBuffer.empty[Int]
              val calls = scala.collection.mutable.ArrayBuffer.empty[Json]
              override def visitMethodInsn(opcode: Int, owner: String, name: String, descriptor: String, isInterface: Boolean): Unit =
                calls += Json.obj("owner" -> Json.fromString(owner), "name" -> Json.fromString(name), "descriptor" -> Json.fromString(descriptor))
              override def visitLineNumber(line: Int, start: Label): Unit = lines += line
              override def visitEnd(): Unit =
                methods += Json.obj("name" -> Json.fromString(name), "descriptor" -> Json.fromString(descriptor),
                  "access" -> Json.fromInt(access), "calls" -> Json.arr(calls.toVector*), "lines" -> Json.arr(lines.distinct.sorted.map(Json.fromInt).toSeq*))
        , ClassReader.SKIP_FRAMES)
        declarations += Json.obj("name" -> Json.fromString(reader.getClassName), "module" -> Json.fromString(module),
          "source" -> Json.fromString(source), "access" -> Json.fromInt(reader.getAccess),
          "parent" -> Json.fromString(Option(reader.getSuperName).getOrElse("")),
          "interfaces" -> Json.arr(reader.getInterfaces.toVector.map(Json.fromString)*),
          "methods" -> Json.arr(methods.toVector*))
      }
      finally stream.close()
    val aggregate = new ExecFileLoader
    val hits = scala.collection.mutable.Map.empty[(String, String, String), scala.collection.mutable.Set[String]]
    val execRoot = root.resolve("target/api-audit/execution")
    val stream = Files.walk(execRoot)
    val files = try stream.iterator.asScala.filter(_.toString.endsWith(".exec")).toVector.sortBy(_.toString) finally stream.close()
    for file <- files do
      val loader = new ExecFileLoader; loader.load(file.toFile)
      aggregate.load(file.toFile)
      val coverage = new CoverageBuilder
      val analyzer = new Analyzer(loader.getExecutionDataStore, coverage)
      loader.getExecutionDataStore.getContents.asScala.foreach { entry =>
        classes.get(entry.getName).foreach { (path, _) => analyzer.analyzeClass(Files.readAllBytes(path), path.toString) }
      }
      val suite = execRoot.relativize(file).toString.stripSuffix(".exec")
      coverage.getClasses.asScala.foreach { c =>
        require(!c.isNoMatch, s"Stale instrumented class ${c.getName} in $suite")
        c.getMethods.asScala.filter(_.getInstructionCounter.getCoveredCount > 0).foreach { m =>
          hits.getOrElseUpdate((c.getName, m.getName, m.getDesc), scala.collection.mutable.Set.empty) += suite
        }
      }
    val invoked = hits.toVector.sortBy(_._1).map { case ((owner, name, descriptor), suites) =>
      Json.obj("owner" -> Json.fromString(owner), "name" -> Json.fromString(name), "descriptor" -> Json.fromString(descriptor),
        "suites" -> Json.arr(suites.toVector.sorted.map(Json.fromString)*))
    }
    val combined = new CoverageBuilder
    val analyzer = new Analyzer(aggregate.getExecutionDataStore, combined)
    classes.values.foreach { (path, _) => analyzer.analyzeClass(Files.readAllBytes(path), path.toString) }
    val branches = combined.getClasses.asScala.toVector.flatMap { c =>
      c.getMethods.asScala.toVector.map { m =>
        Json.obj("owner" -> Json.fromString(c.getName), "name" -> Json.fromString(m.getName),
          "descriptor" -> Json.fromString(m.getDesc),
          "covered" -> Json.fromInt(m.getBranchCounter.getCoveredCount),
          "missed" -> Json.fromInt(m.getBranchCounter.getMissedCount))
      }
    }.sortBy(_.noSpaces)
    val output = Path.of(args(1)); Files.createDirectories(output.getParent)
    Files.writeString(output, Json.obj("classes" -> Json.arr(declarations.toVector.sortBy(_.hcursor.get[String]("name").toOption.get)*),
      "branches" -> Json.arr(branches*), "invocations" -> Json.arr(invoked*), "suiteFiles" -> Json.fromInt(files.size)).noSpaces + "\n")
    println(s"${classes.size} JVM classes; ${files.size} suite files; ${hits.size} invoked methods")
