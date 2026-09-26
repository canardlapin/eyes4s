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

/** Studio boundary rules (DESIGN_SPEC section 13, ticket S0.2).
  *
  * Three rules keep Eyes Studio portable by construction:
  *
  *   1. No eyes4s library module depends on a studio project, directly or
  *      transitively ([[libraryToStudioEdges]]).
  *   2. The portable studio projects (studio-core, studio-app, studio-viz)
  *      resolve no JavaFX artifact ([[forbiddenInPortableStudio]]).
  *   3. Portable studio sources name no `javafx`, `scaladock.fx`, `java.io` or
  *      `java.nio` package, in an import or a qualified reference
  *      ([[scanSource]]).
  *
  * Every rule is a pure function so that [[selfTest]] can run it against
  * planted violations. `checkBoundaries` runs the self-test before the real
  * check, so a rule that has stopped detecting anything fails the build rather
  * than passing silently.
  */
object StudioLint {

  /** Package prefixes that only studio-desktop may name. */
  val forbiddenPackages: Seq[String] = Seq("javafx", "scaladock.fx", "java.io", "java.nio")

  /** Source roots, relative to the build root, that must stay portable. */
  val portableSourceRoots: Seq[String] = Seq("studio/core", "studio/app", "studio/viz")

  /** One forbidden package reference, located by file and line. */
  final case class Violation(file: String, line: Int, reference: String) {
    def render: String = s"$file:$line  '$reference'"
  }

  // A reference starts at an identifier boundary: `eyes4s.javafx` or `myjava.io`
  // is not the JDK package. `_root_.` is allowed in front.
  private val referencePattern =
    forbiddenPackages
      .map(p => java.util.regex.Pattern.quote(p))
      .mkString("(?<![\\w.$])(?:_root_\\.)?(", "|", ")(?![\\w$])")
      .r

  private def blankOut(m: scala.util.matching.Regex.Match): String =
    m.matched.map(c => if (c == '\n') '\n' else ' ')

  /** Blank comments and string literals, preserving newlines for line numbers. */
  def stripCommentsAndStrings(source: String): String = {
    val tripleQuoted = "(?s)\"\"\".*?\"\"\"".r
    val quoted       = "\"(?:\\\\.|[^\"\\\\\n])*\"".r
    val block        = "(?s)/\\*.*?\\*/".r
    val line         = "//[^\n]*".r
    Seq(tripleQuoted, quoted, block, line).foldLeft(source) { (s, r) =>
      r.replaceAllIn(s, m => scala.util.matching.Regex.quoteReplacement(blankOut(m)))
    }
  }

  /** Every forbidden package reference in one source text. */
  def scanSource(fileName: String, source: String): Seq[Violation] = {
    val code = stripCommentsAndStrings(source)
    referencePattern
      .findAllMatchIn(code)
      .map { m =>
        Violation(fileName, code.take(m.start).count(_ == '\n') + 1, m.matched)
      }
      .toList
  }

  /** Every forbidden package reference under the portable source roots. */
  def scanTree(buildRoot: File): Seq[Violation] =
    portableSourceRoots.flatMap { root =>
      val dir     = buildRoot / root
      val sources =
        if (dir.exists) (dir ** "*.scala").get.filterNot(_.getPath.contains("/target/"))
        else Nil
      sources.sortBy(_.getPath).flatMap { f =>
        val relative = IO.relativize(buildRoot, f).getOrElse(f.getPath)
        scanSource(relative, IO.read(f))
      }
    }

  /** A resolved artifact that a portable studio project must not carry. */
  def forbiddenInPortableStudio(organization: String, name: String): Boolean =
    organization == "org.openjfx" ||
      name.startsWith("javafx") ||
      name.contains("-javafx") ||
      name.startsWith("scaladock-fx")

  /** Every library-to-studio dependency path in a project graph.
    *
    * `graph` maps a project id to the ids it depends on directly. A project is
    * a studio project when `isStudio` says so; every other project reachable in
    * the graph is treated as a library module. A path is reported once per
    * library project, as the shortest chain ending at a studio project.
    */
  def libraryToStudioEdges(
      graph: Map[String, Seq[String]],
      isStudio: String => Boolean
  ): Seq[Seq[String]] = {
    def shortestPath(from: String): Option[Seq[String]] = {
      @annotation.tailrec
      def bfs(frontier: List[Seq[String]], seen: Set[String]): Option[Seq[String]] =
        frontier match {
          case Nil          => None
          case path :: rest =>
            val next = graph.getOrElse(path.last, Nil).filterNot(seen)
            next.find(isStudio) match {
              case Some(hit) => Some(path :+ hit)
              case None      => bfs(rest ++ next.map(path :+ _), seen ++ next)
            }
        }
      bfs(List(Seq(from)), Set(from))
    }
    graph.keys.toList.sorted.filterNot(isStudio).flatMap(shortestPath)
  }

  /** Planted violations every rule must detect, and clean inputs it must pass.
    *
    * Returns the failures of the rules themselves; an empty result means every
    * rule still bites.
    */
  def selfTest: Seq[String] = {
    val failures                                  = Seq.newBuilder[String]
    def expect(cond: Boolean, what: String): Unit = if (!cond) failures += what

    // Rule 3: the source lint.
    val planted = Seq(
      "import javafx.scene.control.Label"                    -> "javafx",
      "import scaladock.fx.Dock"                             -> "scaladock.fx",
      "import java.io.File"                                  -> "java.io",
      "import java.nio.file.Files"                           -> "java.nio",
      "import java.nio.file.{Files, Path}"                   -> "java.nio",
      "import cats.syntax.all.*, java.io.File"               -> "java.io",
      "import _root_.java.io.File"                           -> "_root_.java.io",
      "val f = new java.io.File(\"x\")"                      -> "java.io",
      "def p: java.nio.file.Path = ???"                      -> "java.nio",
      "object X:\n  import javafx.application.Platform"      -> "javafx",
      "import scaladock.fx\nimport scaladock.fx.{Dock as D}" -> "scaladock.fx"
    )
    planted.foreach { case (source, reference) =>
      val found = scanSource("planted.scala", source)
      expect(
        found.exists(_.reference == reference),
        s"source lint missed '$reference' in: ${source.replace('\n', ' ')}"
      )
    }
    val multiLine = scanSource("planted.scala", "package p\n\n// ok\nimport java.io.File\n")
    expect(multiLine.map(_.line) == Seq(4), s"source lint misreported a line: $multiLine")

    val clean = Seq(
      "import java.util.UUID",
      "import eyes4s.studio.javafxless.Model",
      "import my.java.io.Thing",
      "// import javafx.scene.Node",
      "/* java.nio.file.Files */ val x = 1",
      "val s = \"java.io.File\"",
      "val t = \"\"\"javafx.scene\n java.nio\"\"\"",
      "import java.iox.Stream",
      "import scaladock.core.Layout"
    )
    clean.foreach { source =>
      val found = scanSource("clean.scala", source)
      expect(found.isEmpty, s"source lint flagged clean source: ${source.replace('\n', ' ')}")
    }

    // Rule 2: resolved artifacts of the portable studio projects.
    expect(forbiddenInPortableStudio("org.openjfx", "javafx-base"), "openjfx base not rejected")
    expect(forbiddenInPortableStudio("org.openjfx", "javafx-controls"), "openjfx not rejected")
    expect(
      forbiddenInPortableStudio("io.example", "intaglio-javafx_3"),
      "a -javafx adapter passed"
    )
    expect(forbiddenInPortableStudio("io.example", "scaladock-fx_3"), "scaladock-fx passed")
    expect(!forbiddenInPortableStudio("org.typelevel", "cats-core_3"), "cats-core rejected")
    expect(
      !forbiddenInPortableStudio("io.example", "scaladock-core_3"),
      "scaladock-core rejected"
    )

    // Rule 1: library-to-studio edges, direct and transitive.
    val isStudio: String => Boolean = _.startsWith("studio")
    val healthy                     = Map(
      "kernelJVM"     -> Seq.empty,
      "planJVM"       -> Seq("kernelJVM"),
      "studioCoreJVM" -> Seq("planJVM"),
      "studioDesktop" -> Seq("studioCoreJVM")
    )
    expect(libraryToStudioEdges(healthy, isStudio).isEmpty, "healthy graph was rejected")
    val direct = healthy.updated("planJVM", Seq("kernelJVM", "studioCoreJVM"))
    expect(
      libraryToStudioEdges(direct, isStudio).contains(Seq("planJVM", "studioCoreJVM")),
      "direct library-to-studio edge was missed"
    )
    val transitive = healthy ++ Map(
      "ioJVM"     -> Seq("bridgeJVM"),
      "bridgeJVM" -> Seq("studioDesktop")
    )
    expect(
      libraryToStudioEdges(transitive, isStudio)
        .contains(Seq("ioJVM", "bridgeJVM", "studioDesktop")),
      "transitive library-to-studio path was missed"
    )

    failures.result()
  }
}
