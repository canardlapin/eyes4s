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

package eyes4s.studio.core.real

import eyes4s.codec.{ArtifactName, ByteDigest}
import eyes4s.studio.core.artifacts.*
import eyes4s.studio.core.assets.AssetRegistry
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.*
import eyes4s.studio.core.fixture.StoryMoments
import io.circe.Json
import java.lang.management.ManagementFactory
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

/** Actual native save and cold restoration; raw CSV sources are never saved. */
object NativeMemoryCourt:
  private def get[E, A](value: Either[E, A]): A =
    value.fold(e => throw new IllegalStateException(e.toString), identity)
  private def phase[A](name: String)(run: => A): A =
    System.gc()
    val started = System.nanoTime()
    val value   = run
    val elapsed = (System.nanoTime() - started) / 1000000L
    val garbage = ManagementFactory.getMemoryMXBean.getHeapMemoryUsage.getUsed
    System.gc()
    println(
      "NATIVE_MEMORY=" + Json
        .obj(
          "phase"           -> Json.fromString(name),
          "elapsed_ms"      -> Json.fromLong(elapsed),
          "live_heap_bytes" -> Json.fromLong(
            ManagementFactory.getMemoryMXBean.getHeapMemoryUsage.getUsed
          ),
          "heap_with_garbage_bytes" -> Json.fromLong(garbage),
          "max_heap_bytes"          -> Json.fromLong(Runtime.getRuntime.maxMemory()),
          "process_id"              -> Json.fromLong(ManagementFactory.getRuntimeMXBean.getPid)
        )
        .noSpaces
    )
    value

  def main(args: Array[String]): Unit =
    require(args.length >= 2, "write|read DIRECTORY [PARTICIPANTS STIMULI NX NY]")
    require(Runtime.getRuntime.maxMemory() <= 1073741824L, "run in a fresh JVM with -Xmx1g")
    val directory = Path.of(args(1))
    if args(0) == "write" then
      val _            = Files.createDirectories(directory)
      val participants = args.lift(2).fold(5)(_.toInt)
      val stimuli      = args.lift(3).fold(10)(_.toInt)
      val nx           = args.lift(4).fold(256)(_.toInt)
      val ny           = args.lift(5).fold(128)(_.toInt)
      val csv          = phase("sources") {
        val fixes = new StringBuilder(
          "participant,phase,trial,occurrence,ordinal,onset_ms,duration_ms,x,y,sample_count\n"
        )
        val trials = new StringBuilder(
          "participant,phase,trial,occurrence,item,response,display_kind,image_file\n"
        )
        for p <- 0 until participants; item <- 0 until stimuli; ph <- 0 until 3 do
          val name  = Vector("Encoding", "Retrieval", "Other")(ph)
          val trial = s"trial-$ph-$item"
          trials.append(s"P$p,$name,$trial,1,item-$item,Remembered,image,item-$item.png\n")
          for index <- 0 until 32 do
            val x = 580 + ((p * 13 + item * 7 + ph * 3 + index * 11)  % 40)
            val y = 280 + ((p * 17 + item * 11 + ph * 7 + index * 13) % 40)
            fixes.append(s"P$p,$name,$trial,1,${index + 1},${index * 160},100,$x,$y,1\n")
        (fixes.result(), trials.result())
      }
      val sample   = get(StoryMoments.t2)
      val original = sample.datasets.last
      val byRole   = Map(SourceRole.Fixations -> csv._1, SourceRole.Trials -> csv._2)
      val sources  = get(
        Sources.of(
          original.sources.entries.map(s =>
            s.copy(
              bytes = ByteDigest.sha256(IArray.from(byRole(s.role).getBytes(UTF_8))),
              semantic = None
            )
          )
        )
      )
      val spec   = original.copy(parent = None, sources = sources)
      val assets = get(
        AssetRegistry.of(spec.id, sources.trials.get.bytes, spec.geometry.screen, Vector.empty)
      )
      val admitted = phase("admission")(get(RealAdmission.admit(spec, csv._1, csv._2, assets)))
      val recipe   = sample.analyses.last.recipe.copy(
        grid = get(GridSize.of(nx, ny)),
        scales = get(ScaleSet.of(Vector(1.0, 1.5, 2.0, 2.5).map(s => get(Sigma.of(s)))))
      )
      val prepared = get(RealPrepared.of(AnalysisRevision(1), spec.id, recipe, admitted))
      val result   = phase("analysis")(get(prepared.work.run))
      val held = RealStudyBackend.RealRun(prepared, result, RealStudyBackend.RunOrigin.Computed)
      val pkg  = phase("native-package")(
        get(NativeArtifacts.build(RunId(1), held, NativeArtifactBudget.Default))
      )
      val revision = AnalysisRevisionSpec(
        prepared.revision,
        spec.id,
        CoreBinding.Bound(pkg.facts.planCanonical),
        prepared.recipe.copy(input = Some(pkg.facts.source)),
        sample.analyses.last.studio.copy(preset = Preset.Custom)
      )
      val ref = RunRef(
        RunId(1),
        revision.id,
        spec.id,
        RunLifecycle.Completed,
        CoreBinding.Bound(pkg.facts.result)
      )
      val document = get(
        StudioDocument.of(
          Vector(spec),
          Vector(revision),
          None,
          Vector(ref),
          Vector.empty,
          Vector.empty,
          PresentationState.default,
          Vector.empty
        )
      )
      phase("save") {
        val entries = pkg.archive.files.map { (entry, bytes) =>
          val _ = Files.write(
            directory.resolve(entry.sha256.hex + ".bin"),
            bytes.asInstanceOf[Array[Byte]]
          )
          Json.obj(
            "name" -> Json.fromString(entry.name.value),
            "sha"  -> Json.fromString(entry.sha256.hex)
          )
        }
        val index = Json.obj(
          "format"   -> Json.fromString("eyes4s.native-memory-court/1"),
          "document" -> get(StudioDocument.encode(document)),
          "files"    -> Json.arr(entries*)
        )
        val _ = Files.writeString(directory.resolve("index.json"), index.noSpaces, UTF_8)
      }
      require(held.result.scales.map(_.estimation.size).sum == participants * stimuli * 3 * 4)
      println(
        "NATIVE_MEMORY_SUMMARY=" + Json
          .obj(
            "mode"          -> Json.fromString("write"),
            "trials"        -> Json.fromInt(participants * stimuli * 3),
            "nx"            -> Json.fromInt(nx),
            "ny"            -> Json.fromInt(ny),
            "maps"          -> Json.fromInt(result.scales.map(_.estimation.size).sum),
            "archive_bytes" -> Json.fromLong(pkg.archive.index.contentBytes),
            "result"        -> Json.fromString(pkg.facts.result.display)
          )
          .noSpaces
      )
    else if args(0) == "read" then
      val index = phase("open")(
        get(io.circe.parser.parse(Files.readString(directory.resolve("index.json"))))
      )
      require(get(index.hcursor.get[String]("format")) == "eyes4s.native-memory-court/1")
      val document = get(StudioDocument.decode(get(index.hcursor.get[Json]("document"))))
      val files    = phase("files") {
        get(index.hcursor.get[Vector[Json]]("files")).map { entry =>
          val name = get(ArtifactName.of(get(entry.hcursor.get[String]("name"))))
          val sha  = get(ByteDigest.parse(get(entry.hcursor.get[String]("sha"))))
          name -> NativeArtifactBytes.copyOf(
            IArray.unsafeFromArray(Files.readAllBytes(directory.resolve(sha.hex + ".bin")))
          )
        }
      }
      val pkg      = phase("verify-native-package")(get(NativeArtifactPackage.verify(files)))
      val ref      = document.runs.head
      val restored = phase("source-free-restore")(
        get(NativeArtifactRestore.of(pkg, ref, document.analyses.head, document.datasets.head))
      )
      require(restored.origin == RealStudyBackend.RunOrigin.Restored(pkg.manifestAddress))
      println(
        "NATIVE_MEMORY_SUMMARY=" + Json
          .obj(
            "mode"          -> Json.fromString("read"),
            "source_free"   -> Json.fromBoolean(true),
            "maps"          -> Json.fromInt(restored.result.scales.map(_.estimation.size).sum),
            "archive_bytes" -> Json.fromLong(pkg.archive.index.contentBytes),
            "result"        -> Json.fromString(pkg.facts.result.display)
          )
          .noSpaces
      )
    else throw new IllegalArgumentException("mode must be write or read")
