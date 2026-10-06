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

import scala.quoted.*
import scala.tasty.inspector.*
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import io.circe.Json

object Inventory:
  def main(args: Array[String]): Unit =
    require(args.length == 4, "Inventory ROOT PLATFORM CLASSPATH_FILE OUTPUT")
    val root = Path.of(args(0)).toAbsolutePath.normalize
    val axis = args(1)
    val modules = Vector("kernel", "core", "detect", "surface", "aoi", "compare", "design", "plan", "results", "codec", "laws", "fs2", "io")
    val files = modules.flatMap { module =>
      val stream = Files.walk(root.resolve(s"$module/.$axis/target/scala-3.7.4/classes"))
      try stream.iterator.asScala.filter(_.toString.endsWith(".tasty")).map(_.toString).toVector
      finally stream.close()
    }
    val classpath = Files.readAllLines(Path.of(args(2))).asScala.toList
    val result = inspect(root, axis, files.toList, classpath)
    val output = Path.of(args(3)); Files.createDirectories(output.getParent)
    Files.writeString(output, result.spaces2 + "\n")
    println(s"Wrote ${result.hcursor.downField("entries").focus.get.asArray.get.size} compiler declarations to $output")

  private[audittool] def inspect(root: Path, axis: String, files: List[String], classpath: List[String]): Json =
    val inspector = new Collector(root)
    require(TastyInspector.inspectAllTastyFiles(files, Nil, classpath)(inspector), "TASTy inspection failed")
    val sorted = inspector.rows.toVector.distinct.sortBy(j => j.hcursor.get[String]("id").toOption.get)
    Json.obj("platform" -> Json.fromString(axis), "entries" -> Json.arr(sorted*))

  private[audittool] final case class DeclarationPosition(source: String, line: Int, endLine: Int)

  private[audittool] def declarationPosition(using quotes: Quotes)(
      tree: quotes.reflect.Tree, origin: DeclarationOrigin
  ): DeclarationPosition =
    DeclarationPosition(origin.path, origin.lineAt(tree.pos.start), origin.lineAt(tree.pos.end))

  private[audittool] final class DeclarationOrigin(root: Path, source: String):
    private val file = root.resolve(source).normalize
    val path: String =
      val normalized = file.toString.replace('\\', '/')
      if normalized.startsWith(root.toString + "/") then normalized.substring(root.toString.length + 1)
      else normalized
    private val text = Files.readString(file)
    // Compiler spans use UTF-16 offsets. Keep CRLF together, following Scala's
    // source line convention; form feed and substitute also terminate a line.
    private val lineStarts =
      (Vector(0) ++ text.indices.collect {
        case i if text(i) == '\n' || text(i) == '\f' || text(i) == '\u001a' ||
          (text(i) == '\r' && (i + 1 == text.length || text(i + 1) != '\n')) => i + 1
      }).toArray
    def lineAt(offset: Int): Int =
      require(offset >= 0 && offset <= text.length, s"Declaration offset $offset outside $path (${text.length} UTF-16 code units)")
      val found = java.util.Arrays.binarySearch(lineStarts, offset)
      if found >= 0 then found + 1 else -found - 1

  private[audittool] class Collector(root: Path) extends Inspector:
    val rows = scala.collection.mutable.ArrayBuffer.empty[Json]
    def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit =
      import quotes.reflect.*
      val origins = scala.collection.mutable.Map.empty[String, DeclarationOrigin]
      def enclosing(s: Symbol): List[Symbol] =
        if !s.exists || s.isPackageDef then Nil else s :: enclosing(s.owner)
      def signature(s: Symbol): String =
        if s.isDefDef then s.signature.paramSigs.mkString("(", ",", ")") + ":" + s.signature.resultSig
        else ""
      def binaryName(s: Symbol): String =
        if !s.exists || s.isPackageDef then s.fullName.replace('.', '/')
        else if s.owner.isPackageDef then s.fullName.replace('.', '/')
        else binaryName(s.owner) + (if s.owner.name.endsWith("$") then "" else "$") + s.name
      def emit(tree: Tree, kind: String, origin: DeclarationOrigin): Unit =
        val s = tree.symbol
        val chain = enclosing(s)
        val restricted = chain.exists(x => x.flags.is(Flags.Private) || x.privateWithin.nonEmpty)
        val local = chain.drop(1).exists(x => x.isDefDef || (x.isValDef && !x.flags.is(Flags.Module)) || x.isAnonymousClass || x.isAnonymousFunction)
        val parameter = s.flags.is(Flags.Param) || s.isTypeParam
        if !local && !parameter && s.fullName.startsWith("eyes4s.") then
          // Lazily unpickled member trees can retain the source context of a
          // referring compilation unit (bead bd-01M47691E846DHVK0A352R4EGR).
          // Their spans still belong to the declaration. Resolve paths and line
          // numbers from the owning TASTy's source; reflected line tables can
          // also be seeded from the referring file's TASTy positions.
          val position = declarationPosition(tree, origin)
          val category =
            if restricted then "internal"
            else if s.flags.is(Flags.Synthetic) || s.flags.is(Flags.Artifact) then "compiler-generated"
            else if kind == "type" then "type-level"
            else if kind == "class" then "abstraction-or-container"
            else if s.flags.is(Flags.Deferred) then "abstract-member"
            else if s.flags.is(Flags.Inline) then "inline-member"
            else "runtime-entry"
          rows += Json.obj(
            "id" -> Json.fromString(s.fullName + signature(s)), "name" -> Json.fromString(s.name),
            "owner" -> Json.fromString(s.owner.fullName),
            "binaryOwner" -> Json.fromString(binaryName(s.owner)),
            "binaryName" -> Json.fromString(binaryName(s)), "kind" -> Json.fromString(kind),
            "ownerFlags" -> Json.fromString(s.owner.flags.show),
            "valueType" -> Json.fromString(tree match { case v: ValDef => v.tpt.tpe.show; case _ => "" }),
            "sourceType" -> Json.fromString(tree match
              case d: DefDef =>
                d.paramss.map(_.params.map {
                  case v: ValDef => v.name + ": " + v.tpt.tpe.show
                  case t: TypeDef => t.name + ": " + t.rhs.show
                }.mkString("(", ",", ")")).mkString + ": " + d.returnTpt.tpe.show
              case v: ValDef => v.tpt.tpe.show
              case c: ClassDef =>
                c.constructor.paramss.map(_.params.map {
                  case v: ValDef => v.name + ": " + v.tpt.tpe.show
                  case t: TypeDef => t.name + ": " + t.rhs.show
                }.mkString("(", ",", ")")).mkString + " extends " + c.parents.map(_.show).mkString(" & ")
              case t: TypeDef => t.rhs.show
              case _ => ""),
            "signature" -> Json.fromString(signature(s)), "flags" -> Json.fromString(s.flags.show),
            "category" -> Json.fromString(category), "source" -> Json.fromString(position.source),
            "line" -> Json.fromInt(position.line), "endLine" -> Json.fromInt(position.endLine),
            "deprecated" -> Json.fromBoolean(s.annotations.exists(_.tpe.typeSymbol.fullName == "scala.deprecated")),
            "overrides" -> Json.arr(s.allOverriddenSymbols.map(x => Json.fromString(x.fullName)).toSeq*),
            "inherited" -> (tree match
              case _: ClassDef => Json.arr(s.methodMembers.filter(m => m.owner != s && !m.flags.is(Flags.Private) && m.privateWithin.isEmpty).map(m => Json.fromString(m.fullName + signature(m)))*)
              case _ => Json.arr()),
            "parents" -> (tree match
              case c: ClassDef => Json.arr(c.parents.map(p => Json.fromString(p.show))*)
              case _ => Json.arr())
          )
      def traverse(tasty: Tasty[quotes.type]): Unit =
        val source = tasty.ast.pos.sourceFile.path
        val origin = origins.getOrElseUpdate(source, new DeclarationOrigin(root, source))
        val traverser = new TreeTraverser:
          override def traverseTree(tree: Tree)(owner: Symbol): Unit =
            tree match
              case _: ClassDef => emit(tree, "class", origin)
              case _: DefDef => emit(tree, "method", origin)
              case _: ValDef => emit(tree, "value", origin)
              case _: TypeDef => emit(tree, "type", origin)
              case _ => ()
            super.traverseTree(tree)(owner)
        traverser.traverseTree(tasty.ast)(Symbol.spliceOwner)
      tastys.foreach(traverse)
