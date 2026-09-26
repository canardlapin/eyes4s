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
  * Four rules keep Eyes Studio portable by construction:
  *
  *   1. No eyes4s library module depends on a studio project, directly or
  *      transitively ([[libraryToStudioEdges]]).
  *   2. The portable studio projects (studio-core, studio-app, studio-viz)
  *      resolve no JavaFX artifact ([[forbiddenInPortableStudio]]).
  *   3. Portable studio sources name no `javafx`, `scaladock.fx`, `java.io`,
  *      `java.nio.file` or `java.nio.channels` package ([[forbiddenPackages]]).
  *   4. The pure presentation layers (studio-app, studio-viz) name no
  *      `cats.effect` or `fs2` package: effects are data there
  *      ([[effectPackages]], [[pureSourceRoots]]).
  *
  * Rules 3 and 4 share [[scanSource]]. It reads imports structurally, so a
  * selector (`java.{io as jio}`), a rename, or a wildcard that brings a
  * forbidden package into scope (`java.*`, `cats._`) is caught, and it scans
  * the expressions inside string interpolations (`s"${...}"`) as code.
  *
  * Every rule is a pure function so that [[selfTest]] can run it against
  * planted violations. `checkBoundaries` runs the self-test before the real
  * check, so a rule that has stopped detecting anything fails the build rather
  * than passing silently.
  */
object StudioLint {

  /** Packages that only studio-desktop may name. `java.nio` buffers and
    * charsets are portable and stay allowed; its file system and channels are
    * platform services.
    */
  val forbiddenPackages: Seq[String] =
    Seq("javafx", "scaladock.fx", "java.io", "java.nio.file", "java.nio.channels")

  /** Source roots, relative to the build root, that must stay portable. */
  val portableSourceRoots: Seq[String] = Seq("studio/core", "studio/app", "studio/viz")

  /** Effect-library packages the pure presentation layers may not name. */
  val effectPackages: Seq[String] = Seq("cats.effect", "fs2")

  /** Source roots, relative to the build root, that must stay effect-free:
    * effects are data there (DESIGN_SPEC section 13).
    */
  val pureSourceRoots: Seq[String] = Seq("studio/app", "studio/viz")

  /** One forbidden package reference, located by file and line. */
  final case class Violation(file: String, line: Int, reference: String) {
    def render: String = s"$file:$line  '$reference'"
  }

  // ---------------------------------------------------------------------------
  // Lexing: keep code, blank everything else, preserve every offset.
  // ---------------------------------------------------------------------------

  private def isIdentChar(c: Char): Boolean =
    Character.isLetterOrDigit(c) || c == '_' || c == '$'

  /** The source with comments and literal text blanked and newlines kept, so
    * offsets and line numbers still refer to the real file. The expressions of
    * a string interpolation (`${...}`) stay code; nested literals inside them
    * are blanked in turn.
    */
  def codeOnly(source: String): String = {
    val n   = source.length
    val out = new StringBuilder(n)

    def blank(c: Char): Unit = out.append(if (c == '\n') '\n' else ' ')

    def blockComment(start: Int): Int = {
      // Called at an opening "/*"; Scala block comments nest.
      var i     = start + 2
      var depth = 1
      blank(' '); blank(' ')
      while (depth > 0 && i < n) {
        if (source.startsWith("/*", i)) { depth += 1; blank(' '); blank(' '); i += 2 }
        else if (source.startsWith("*/", i)) { depth -= 1; blank(' '); blank(' '); i += 2 }
        else { blank(source(i)); i += 1 }
      }
      i
    }

    def charLiteral(start: Int): Int =
      if (start + 1 < n && source(start + 1) == '\\') {
        val close = source.indexOf('\'', start + 2)
        val end   = if (close < 0 || close - start > 8) start + 1 else close + 1
        (start until end).foreach(i => blank(source(i)))
        end
      } else if (start + 2 < n && source(start + 2) == '\'') {
        (start until start + 3).foreach(i => blank(source(i)))
        start + 3
      } else { out.append('\''); start + 1 } // a quote, a type quote or a symbol

    def stringLiteral(start: Int, interpolated: Boolean): Int = {
      val triple = source.startsWith("\"\"\"", start)
      var i      = start + (if (triple) 3 else 1)
      (start until i).foreach(_ => blank(' '))
      while (i < n) {
        if (triple && source.startsWith("\"\"\"", i)) {
          var end = i + 3
          while (end < n && source(end) == '"') end += 1
          (i until end).foreach(_ => blank(' '))
          return end
        } else if (!triple && source(i) == '"') { blank(' '); return i + 1 }
        else if (!triple && source(i) == '\n') return i // unterminated
        else if (!triple && source(i) == '\\' && i + 1 < n) {
          blank(' '); blank(source(i + 1)); i += 2
        } else if (interpolated && source.startsWith("$$", i)) {
          blank(' '); blank(' '); i += 2
        } else if (interpolated && source.startsWith("${", i)) {
          out.append("${")
          i = code(i + 2, nested = true)
          if (i < n) { out.append('}'); i += 1 }
        } else { blank(source(i)); i += 1 }
      }
      i
    }

    // Returns the offset of the brace that closes a nested block, or n.
    def code(from: Int, nested: Boolean): Int = {
      var i     = from
      var depth = 0
      while (i < n) {
        val c = source(i)
        if (source.startsWith("//", i)) {
          while (i < n && source(i) != '\n') { blank(source(i)); i += 1 }
        } else if (source.startsWith("/*", i)) i = blockComment(i)
        else if (c == '"') i = stringLiteral(i, i > 0 && isIdentChar(source(i - 1)))
        else if (c == '\'') i = charLiteral(i)
        else if (c == '}' && nested && depth == 0) return i
        else {
          if (c == '{') depth += 1
          if (c == '}') depth -= 1
          out.append(c); i += 1
        }
      }
      i
    }

    code(0, nested = false)
    out.toString
  }

  // ---------------------------------------------------------------------------
  // Imports: every path an import clause brings into scope.
  // ---------------------------------------------------------------------------

  /** One imported path, normalised: no `_root_.`, no backticks, no whitespace,
    * a wildcard written `.*`. `offset` is where its import expression starts.
    */
  final case class ImportedPath(path: String, offset: Int)

  private val importKeyword = "(?<![\\w$.])import(?![\\w$])".r

  /** Each import clause as (start of `import`, end of the clause). */
  private def importClauses(code: String): Seq[(Int, Int)] =
    importKeyword
      .findAllMatchIn(code)
      .map { m =>
        var i     = m.end
        var depth = 0
        var done  = false
        while (i < code.length && !done) {
          val c = code(i)
          if (c == '{') depth += 1
          else if (c == '}') depth -= 1
          else if (depth == 0 && c == ';') done = true
          else if (depth == 0 && c == '\n') {
            val before = code.substring(m.end, i).trim
            done = !(before.endsWith(",") || before.endsWith(".") || before.isEmpty)
          }
          if (!done) i += 1
        }
        (m.start, i)
      }
      .toList

  private def normalise(path: String): String = {
    val p = path.replaceAll("[\\s`]", "")
    val q = if (p.startsWith("_root_.")) p.stripPrefix("_root_.") else p
    if (q.endsWith("._") || q.endsWith(".given")) q.substring(0, q.lastIndexOf('.')) + ".*"
    else q
  }

  /** The paths one import expression (`a.b.c`, `a.{b, c as d}`, `a.*`) names. */
  def importedPaths(expression: String, offset: Int): Seq[ImportedPath] = {
    val brace = expression.indexOf('{')
    if (brace >= 0) {
      val prefix = normalise(expression.substring(0, brace)).stripSuffix(".")
      val close  = expression.lastIndexOf('}')
      val inner  =
        expression.substring(brace + 1, if (close > brace) close else expression.length)
      inner.split(",").toSeq.flatMap { selector =>
        val parts = selector.trim.split("\\s+|=>").filter(_.nonEmpty).toSeq
        parts match {
          case Seq()                                       => Nil
          case Seq(name, _*) if name == "*" || name == "_" =>
            Seq(ImportedPath(prefix + ".*", offset))
          case Seq(name, _*) if name == "given" => Seq(ImportedPath(prefix + ".*", offset))
          case Seq(_, rest @ _*) if rest.lastOption.contains("_") => Nil // a hiding selector
          case Seq(name, _*) => Seq(ImportedPath(s"$prefix.${name.replace("`", "")}", offset))
        }
      }
    } else {
      val path = expression.split("\\s+as\\s+|=>").headOption.getOrElse("")
      val p    = normalise(path)
      if (p.isEmpty) Nil else Seq(ImportedPath(p, offset))
    }
  }

  /** Split an import clause body at its top-level commas, with offsets. */
  private def importExpressions(code: String, from: Int, until: Int): Seq[(String, Int)] = {
    val pieces = Seq.newBuilder[(String, Int)]
    var depth  = 0
    var start  = from
    (from until until).foreach { i =>
      code(i) match {
        case '{'               => depth += 1
        case '}'               => depth -= 1
        case ',' if depth == 0 => pieces += ((code.substring(start, i), start)); start = i + 1
        case _                 => ()
      }
    }
    pieces += ((code.substring(start, until), start))
    pieces.result().map { case (raw, at) =>
      val lead = raw.indexWhere(c => !c.isWhitespace)
      if (lead < 0) ("", at) else (raw.trim, at + lead)
    }
  }

  /** An imported path violates a rule when it names a forbidden package, a
    * member of one, or an ancestor through which one comes into scope
    * (`import java.nio` or `import java.*` reaches `java.nio.file`).
    */
  def importViolates(path: String, packages: Seq[String]): Boolean = {
    val base = path.stripSuffix(".*")
    packages.exists(p => base == p || base.startsWith(p + ".") || p.startsWith(base + "."))
  }

  // ---------------------------------------------------------------------------
  // The source lint
  // ---------------------------------------------------------------------------

  private def referencePattern(packages: Seq[String]) =
    packages
      .map(p => java.util.regex.Pattern.quote(p))
      .mkString("(?<![\\w.$])(?:_root_\\.)?(", "|", ")(?![\\w$])")
      .r

  /** Every reference to one of `packages` in one source text: each import that
    * brings one into scope, and each qualified reference in code, including
    * code inside string interpolations.
    */
  def scanSource(
      fileName: String,
      source: String,
      packages: Seq[String] = forbiddenPackages
  ): Seq[Violation] = {
    val code                = codeOnly(source)
    def lineOf(offset: Int) = code.take(offset).count(_ == '\n') + 1
    val clauses             = importClauses(code)
    val importViolations    = clauses.flatMap { case (start, end) =>
      importExpressions(code, start + "import".length, end)
        .flatMap { case (expression, at) => importedPaths(expression, at) }
        .filter(p => importViolates(p.path, packages))
        .map(p => Violation(fileName, lineOf(p.offset), p.path))
    }
    val withoutImports = clauses.foldLeft(code) { case (c, (start, end)) =>
      c.substring(0, start) + c.substring(start, end).map(ch => if (ch == '\n') '\n' else ' ') +
        c.substring(end)
    }
    val referenceViolations = referencePattern(packages)
      .findAllMatchIn(withoutImports)
      .map(m => Violation(fileName, lineOf(m.start), m.matched))
      .toList
    (importViolations ++ referenceViolations).sortBy(v => (v.line, v.reference))
  }

  /** Every reference to one of `packages` under `roots`. */
  def scanTree(
      buildRoot: File,
      roots: Seq[String] = portableSourceRoots,
      packages: Seq[String] = forbiddenPackages
  ): Seq[Violation] =
    roots.flatMap { root =>
      val dir     = buildRoot / root
      val sources =
        if (dir.exists) (dir ** "*.scala").get.filterNot(_.getPath.contains("/target/"))
        else Nil
      sources.sortBy(_.getPath).flatMap { f =>
        val relative = IO.relativize(buildRoot, f).getOrElse(f.getPath)
        scanSource(relative, IO.read(f), packages)
      }
    }

  // ---------------------------------------------------------------------------
  // Resolved artifacts and the project graph
  // ---------------------------------------------------------------------------

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

  // ---------------------------------------------------------------------------
  // Self-test
  // ---------------------------------------------------------------------------

  /** Planted violations every rule must detect, and clean inputs it must pass.
    *
    * Returns the failures of the rules themselves; an empty result means every
    * rule still bites.
    */
  def selfTest: Seq[String] = {
    val failures                                  = Seq.newBuilder[String]
    def expect(cond: Boolean, what: String): Unit = if (!cond) failures += what
    def show(source: String): String              = source.replace('\n', ' ')

    def mustFind(rule: String, packages: Seq[String], planted: Seq[(String, String)]): Unit =
      planted.foreach { case (source, reference) =>
        val found = scanSource("planted.scala", source, packages)
        expect(
          found.exists(_.reference == reference),
          s"$rule missed '$reference' in: ${show(source)} (found ${found.map(_.reference)})"
        )
      }

    def mustPass(rule: String, packages: Seq[String], clean: Seq[String]): Unit =
      clean.foreach { source =>
        val found = scanSource("clean.scala", source, packages)
        expect(found.isEmpty, s"$rule flagged clean source: ${show(source)} ($found)")
      }

    // Rule 3: JVM-only and JavaFX packages in portable studio sources.
    mustFind(
      "source lint",
      forbiddenPackages,
      Seq(
        "import javafx.scene.control.Label"               -> "javafx.scene.control.Label",
        "import scaladock.fx.Dock"                        -> "scaladock.fx.Dock",
        "import java.io.File"                             -> "java.io.File",
        "import java.nio.file.Files"                      -> "java.nio.file.Files",
        "import java.nio.file.{Files, Path}"              -> "java.nio.file.Files",
        "import java.nio.channels.FileChannel"            -> "java.nio.channels.FileChannel",
        "import cats.syntax.all.*, java.io.File"          -> "java.io.File",
        "import _root_.java.io.File"                      -> "java.io.File",
        "import java.io as jio"                           -> "java.io",
        "object X:\n  import javafx.application.Platform" -> "javafx.application.Platform",
        "import scaladock.fx\nimport scaladock.fx.{Dock as D}" -> "scaladock.fx",
        // Selectors and renames.
        "import java.{io, util}"                  -> "java.io",
        "import java.{util, nio as n}"            -> "java.nio",
        "import java.{io => jio}"                 -> "java.io",
        "import java.{ `io` }"                    -> "java.io",
        "import scaladock.{fx, core}"             -> "scaladock.fx",
        "import java.nio.{file as f, ByteBuffer}" -> "java.nio.file",
        // Wildcards that bring a forbidden package into scope.
        "import java.*"         -> "java.*",
        "import java._"         -> "java.*",
        "import java.nio.*"     -> "java.nio.*",
        "import scaladock.*"    -> "scaladock.*",
        "import java.{util, *}" -> "java.*",
        // Qualified references, including inside interpolations.
        "val f = new java.io.File(\"x\")"                              -> "java.io",
        "def p: java.nio.file.Path = ???"                              -> "java.nio.file",
        "val s = s\"file ${java.io.File(\"x\")} here\""                -> "java.io",
        "val t = s\"\"\"a ${ java.nio.file.Paths.get(\"b\") } c\"\"\"" -> "java.nio.file",
        "val u = f\"${javafx.scene.paint.Color.RED}%s\""               -> "javafx"
      )
    )
    val multiLine = scanSource("planted.scala", "package p\n\n// ok\nimport java.io.File\n")
    expect(multiLine.map(_.line) == Seq(4), s"source lint misreported a line: $multiLine")
    val continued = scanSource("planted.scala", "import java.util.UUID,\n  java.io.File\n")
    expect(
      continued.map(_.line) == Seq(2),
      s"source lint misreported a continued import: $continued"
    )

    mustPass(
      "source lint",
      forbiddenPackages,
      Seq(
        "import java.util.UUID",
        "import eyes4s.studio.javafxless.Model",
        "import my.java.io.Thing",
        "// import javafx.scene.Node",
        "/* java.nio.file.Files */ val x = 1",
        "/* outer /* java.io */ still comment */ val x = 1",
        "val s = \"java.io.File\"",
        "val t = \"\"\"javafx.scene\n java.nio.file\"\"\"",
        "val u = s\"$name java.io.File ${name.size}\"",
        "val c = '\"'; val d = \"java.io\"",
        "import java.iox.Stream",
        "import scaladock.core.Layout",
        "import java.nio.ByteBuffer",
        "import java.nio.charset.StandardCharsets",
        "import java.nio.{ByteBuffer, CharBuffer}",
        "val b = java.nio.ByteBuffer.allocate(8)",
        "import java.{util, time}",
        "import java.{io => _, util}",
        "import java.util.*"
      )
    )

    // Rule 4: effect libraries in the pure presentation layers.
    mustFind(
      "effect lint",
      effectPackages,
      Seq(
        "import cats.effect.IO"                  -> "cats.effect.IO",
        "import cats.effect.{IO, Resource}"      -> "cats.effect.IO",
        "import cats.syntax.all.*, fs2.Stream"   -> "fs2.Stream",
        "import fs2.*"                           -> "fs2.*",
        "import _root_.fs2.Stream"               -> "fs2.Stream",
        "import cats.{effect, syntax}"           -> "cats.effect",
        "import cats.{effect as ce}"             -> "cats.effect",
        "import cats.{effect => ce}"             -> "cats.effect",
        "import cats.*"                          -> "cats.*",
        "import cats._"                          -> "cats.*",
        "def s: fs2.Stream[fs2.Pure, Int] = ???" -> "fs2",
        "val io = cats.effect.IO.unit"           -> "cats.effect",
        "val m = s\"${cats.effect.IO.unit}\""    -> "cats.effect"
      )
    )
    mustPass(
      "effect lint",
      effectPackages,
      Seq(
        "import cats.syntax.all.*",
        "import cats.data.NonEmptyList",
        "import cats.{data, syntax}",
        "import eyes4s.fs2.StudyExecution",
        "import eyes4s.studio.core.fs2x.Thing",
        "// import cats.effect.IO",
        "val s = \"fs2.Stream\"",
        "val t = s\"cats.effect ${count}\""
      )
    )
    expect(
      !pureSourceRoots.contains("studio/core") && pureSourceRoots.contains("studio/app") &&
        pureSourceRoots.contains("studio/viz"),
      s"effect lint covers the wrong source roots: $pureSourceRoots"
    )

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
