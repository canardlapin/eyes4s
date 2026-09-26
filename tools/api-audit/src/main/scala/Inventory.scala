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
    val rows = scala.collection.mutable.ArrayBuffer.empty[Json]
    val inspector = new Inspector:
      def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit =
        import quotes.reflect.*
        def enclosing(s: Symbol): List[Symbol] =
          if !s.exists || s.isPackageDef then Nil else s :: enclosing(s.owner)
        def signature(s: Symbol): String =
          if s.isDefDef then s.signature.paramSigs.mkString("(", ",", ")") + ":" + s.signature.resultSig
          else ""
        def binaryName(s: Symbol): String =
          if !s.exists || s.isPackageDef then s.fullName.replace('.', '/')
          else if s.owner.isPackageDef then s.fullName.replace('.', '/')
          else binaryName(s.owner) + (if s.owner.name.endsWith("$") then "" else "$") + s.name
        def emit(tree: Tree, kind: String): Unit =
          val s = tree.symbol
          val chain = enclosing(s)
          val restricted = chain.exists(x => x.flags.is(Flags.Private) || x.privateWithin.nonEmpty)
          val local = chain.drop(1).exists(x => x.isDefDef || (x.isValDef && !x.flags.is(Flags.Module)) || x.isAnonymousClass || x.isAnonymousFunction)
          val parameter = s.flags.is(Flags.Param) || s.isTypeParam
          if !local && !parameter && s.fullName.startsWith("eyes4s.") then
            val position = tree.pos
            val path = position.sourceFile.path.replace('\\', '/')
            val relative = if path.startsWith(root.toString + "/") then path.substring(root.toString.length + 1) else path
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
              "category" -> Json.fromString(category), "source" -> Json.fromString(relative),
              "line" -> Json.fromInt(position.startLine + 1), "endLine" -> Json.fromInt(position.endLine + 1),
              "deprecated" -> Json.fromBoolean(s.annotations.exists(_.tpe.typeSymbol.fullName == "scala.deprecated")),
              "overrides" -> Json.arr(s.allOverriddenSymbols.map(x => Json.fromString(x.fullName)).toSeq*),
              "inherited" -> (tree match
                case _: ClassDef => Json.arr(s.methodMembers.filter(m => m.owner != s && !m.flags.is(Flags.Private) && m.privateWithin.isEmpty).map(m => Json.fromString(m.fullName + signature(m)))*)
                case _ => Json.arr()),
              "parents" -> (tree match
                case c: ClassDef => Json.arr(c.parents.map(p => Json.fromString(p.show))*)
                case _ => Json.arr())
            )
        val traverser = new TreeTraverser:
          override def traverseTree(tree: Tree)(owner: Symbol): Unit =
            tree match
              case _: ClassDef => emit(tree, "class")
              case _: DefDef => emit(tree, "method")
              case _: ValDef => emit(tree, "value")
              case _: TypeDef => emit(tree, "type")
              case _ => ()
            super.traverseTree(tree)(owner)
        tastys.foreach(t => traverser.traverseTree(t.ast)(Symbol.spliceOwner))
    val classpath = Files.readAllLines(Path.of(args(2))).asScala.toList
    require(TastyInspector.inspectAllTastyFiles(files.toList, Nil, classpath)(inspector), "TASTy inspection failed")
    val sorted = rows.toVector.distinct.sortBy(j => j.hcursor.get[String]("id").toOption.get)
    val output = Path.of(args(3)); Files.createDirectories(output.getParent)
    Files.writeString(output, Json.obj("platform" -> Json.fromString(axis), "entries" -> Json.arr(sorted*)).spaces2 + "\n")
    println(s"Wrote ${sorted.size} compiler declarations to $output")
