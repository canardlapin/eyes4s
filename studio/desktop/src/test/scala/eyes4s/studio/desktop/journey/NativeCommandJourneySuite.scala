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

package eyes4s.studio.desktop.journey

import cats.effect.{IO, Ref, Resource}
import eyes4s.plan.{ResultInspection, ResultRef}
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.artifacts.NativeBindingFacts
import eyes4s.studio.core.bundle.*
import eyes4s.studio.core.document.{CoreBinding, DatasetRevisionSpec, SemanticIdentity, Source}
import eyes4s.studio.core.assets.AssetRegistry
import eyes4s.studio.core.real.DatasetSources
import eyes4s.studio.core.fixture.GoldenScores
import eyes4s.studio.core.headless.NativeHeadlessSession
import eyes4s.studio.core.session.ProjectSession
import eyes4s.studio.desktop.figures.BundleWriter
import eyes4s.studio.desktop.platform.{FileProjectStore, TempDirs}
import eyes4s.studio.desktop.runtime.{DatasetSourceHosts, SessionPort}
import java.nio.file.Files
import scala.concurrent.duration.*

/** Empty-project native route through the actual command/effect/service path. */
class NativeCommandJourneySuite extends munit.CatsEffectSuite:
  import NativeCommandJourneyFixture.*
  override val munitIOTimeout: Duration                            = 10.minutes
  private def future[A](run: => scala.concurrent.Future[A]): IO[A] = IO.fromFuture(IO(run))
  private val owner = get(LockOwner.of("Native command reopen"))

  /** Test-only elapsed phases; timings never change the qualification budget. */
  private def phase[A](name: String)(body: IO[A]): IO[A] =
    for
      start <- IO.monotonic
      _     <- IO.println(s"NATIVE-PHASE start $name")
      value <- body.guarantee(
        IO.monotonic.flatMap(end =>
          IO.println(s"NATIVE-PHASE end $name ${(end - start).toMillis}ms")
        )
      )
    yield value

  test(
    "new project imports, admits, previews, executes, inspects, saves, reopens and exports native science"
  ) {
    TempDirs.resource("eyes4s-native-command-").use { directory =>
      val path = directory.resolve("command.eyes")
      for
        inputs <- phase("fixture-load")(load)
        first  <- phase("initial-execution-readback-save")(
          NativeCommandJourneyScenario.run(inputs, path)
        )
        direct       <- phase("direct-prepare")(inputs.direct(first.document))
        directResult <- phase("direct-execution")(IO.blocking(get(direct.work.run)))
        _ = qualify(first, direct, directResult)
        store    <- FileProjectStore.at[IO](path)
        reopened <- phase("project-bundle-open")(ProjectBundle.open(store).map(get))
        second   <- Resource
          .make(ProjectSession.open(store, owner).map(get))(_.session.close.map(get))
          .flatMap(opened =>
            Resource.make(IO.blocking(SessionPort.start(opened.session)))(port =>
              IO.blocking(port.close())
            )
          )
          .use { port =>
            for
              stored <- phase("load-native-artifacts")(
                port.session.loadNativeArtifacts(run).map(get)
              )
              _ = println(
                s"Native archive qualification: ${stored.files.size} entries, " +
                  s"${stored.files.foldLeft(BigInt(0))((n, file) => n + file._2.size)} bytes, " +
                  s"${stored.manifest.entries.flatMap(_.layout).foldLeft(BigInt(0))((n, layout) => n + layout.count)} density cells"
              )
              _ = qualifyBindings(stored.facts, first, direct, directResult)
              _ = assertEquals(stored.archive.index.run, run)
              _ = assertEquals(
                stored.archive.index.archive,
                first.document.run(run).get.archive
              )
              _ <- phase("verify-stored-digests")(IO.delay {
                stored.archive.files.foreach { (entry, bytes) =>
                  assertEquals(entry.sha256, eyes4s.codec.ByteDigest.sha256(bytes))
                  assertEquals(entry.length, bytes.length.toLong)
                }
              })
              sourceReads <- Ref.of[IO, Int](0)
              host    = DatasetSourceHosts.stored(port)
              counted = new DatasetSources[IO]:
                def bytes(dataset: DatasetRevisionSpec, source: Source) =
                  sourceReads.update(_ + 1).flatMap(_ => host.bytes(dataset, source))
                def assets(dataset: DatasetRevisionSpec): IO[Option[AssetRegistry]] =
                  sourceReads.update(_ + 1).flatMap(_ => host.assets(dataset))
              output <- Resource
                .make(
                  future(
                    NativeHeadlessSession
                      .open(
                        reopened.document,
                        counted,
                        artifactSource = Some(port.nativeArtifactSource)
                      )
                  )
                )(session => future(session.close))
                .use { session =>
                  for
                    summary <- phase("restored-result")(future(session.result(run)).map(get))
                    reads   <- sourceReads.get
                    _ = assertEquals(summary, first.summary)
                    _ = assertEquals(
                      reads,
                      0,
                      "stored scientific reads must not admit raw sources"
                    )
                    captured <- phase("restored-readback-capture")(
                      NativeCommandJourneyReadback.capture(
                        reopened.document,
                        NativeCommandJourneyReadback.Port.from(session)
                      )
                    )
                    jobs <- session.rawBackend.jobs
                    _ = assertEquals(jobs, Vector.empty[JobStatus])
                    _ = assert(
                      captured.provenance.trail.contains(
                        ProvenanceStep.Restored(stored.manifestAddress)
                      )
                    )
                  yield captured
                }
              _ <- Resource
                .make(future(NativeHeadlessSession.open(reopened.document, host)))(session =>
                  future(session.close)
                )
                .use { session =>
                  for
                    summary <- phase("raw-source-fallback-reexecution")(
                      future(session.result(run)).map(get)
                    )
                    provenance <- future(
                      session.provenance(run, ResultAddress.ContrastRow(scale, focus))
                    ).map(get)
                    _ = assertEquals(summary, first.summary)
                    _ = assert(
                      provenance.trail.exists(_.isInstanceOf[ProvenanceStep.Recomputed])
                    )
                  yield ()
                }
            yield output
          }
        written <- IO.async_[Either[String, String]](done =>
          BundleWriter.write(
            directory.resolve("exports"),
            second.exports.map((name, bytes) => name -> IArray.from(bytes)),
            None,
            answer => done(Right(answer))
          )
        )
      yield
        assertEquals(second.canonicalDocument, first.canonicalDocument)
        assertEquals(second.canonicalScience, first.canonicalScience)
        assertEquals(second.rows, first.rows)
        assertEquals(second.report, first.report)
        assertEquals(second.source, first.source)
        assertEquals(second.exports, first.exports)
        assert(second.provenance.trail.exists(_.isInstanceOf[ProvenanceStep.Restored]))
        assert(!second.provenance.trail.exists(_.isInstanceOf[ProvenanceStep.Recomputed]))
        assert(written.isRight, written)
        second.exports.foreach { (name, bytes) =>
          assertEquals(
            Files.readAllBytes(directory.resolve("exports").resolve(name)).toVector,
            bytes
          )
        }
        // S9.4 recheck on the real backend (slice r8 of S3.7): the native
        // methods text is pinned, and its study counts are eyes4s's, as
        // FIXTURE.md records them independently of Studio.
        val methods = second.exports
          .collectFirst {
            case (name, bytes) if name.endsWith("methods.md") =>
              String(bytes.toArray, java.nio.charset.StandardCharsets.UTF_8)
          }
          .getOrElse(fail("no methods.md exported"))
        val golden = FixtureDoc.root.resolve("docs/studio/methods/native-golden-methods.md")
        if sys.env.contains("EYES4S_UPDATE_GOLDENS") then
          Files.createDirectories(golden.getParent)
          Files.writeString(golden, methods, java.nio.charset.StandardCharsets.UTF_8): Unit
        assert(Files.exists(golden), s"missing $golden; run with EYES4S_UPDATE_GOLDENS=1")
        assertNoDiff(
          methods,
          Files.readString(golden, java.nio.charset.StandardCharsets.UTF_8)
        )
        Vector(
          "937 admitted",
          "543 of the 11,311",
          "457 were eligible",
          "454 contributed",
          "3 failed",
          "9 had no matched trial",
          "14 queries were not admitted"
        ).foreach(n => assert(methods.contains(n), s"methods.md does not state $n:\n$methods"))
    }
  }

  private def qualifyBindings(
      facts: NativeBindingFacts,
      output: NativeCommandJourneyReadback.Output,
      direct: eyes4s.studio.core.real.RealPrepared,
      result: eyes4s.studio.core.real.RealExecution.Result
  ): Unit =
    assertEquals(facts.run, run)
    assertEquals(facts.revision, revision)
    assertEquals(facts.dataset, dataset)
    assertEquals(facts.planCanonical.sha256, get(direct.plans.codec.digest(direct.plan)).sha256)
    assertEquals(
      facts.inputCanonical.sha256,
      get(direct.inputs.input.digest(direct.admitted.input)).sha256
    )
    assertEquals(facts.result.sha256, get(direct.results.codec.digest(result)).sha256)
    assertEquals(
      facts.source,
      SemanticIdentity.fromCore(direct.admitted.evidence.source.records)
    )
    assertEquals(
      output.document.analysis(revision).get.plan,
      CoreBinding.Bound(facts.planCanonical)
    )
    assertEquals(output.document.analysis(revision).get.recipe.input, Some(facts.source))
    assertEquals(output.document.run(run).get.archive, CoreBinding.Bound(facts.result))

  private def qualify(
      output: NativeCommandJourneyReadback.Output,
      prepared: eyes4s.studio.core.real.RealPrepared,
      result: eyes4s.studio.core.real.RealExecution.Result
  ): Unit =
    val golden   = get(io.circe.parser.parse(GoldenScores.text)).hcursor
    val expected = get(golden.get[Vector[io.circe.Json]]("queries")).map { q =>
      (
        get(q.hcursor.get[String]("participant")),
        get(q.hcursor.get[String]("trial"))
      ) -> q.hcursor
    }.toMap
    val direct = get(
      ResultInspection.study(
        prepared.plan,
        result,
        prepared.admitted.input,
        Some(prepared.admitted.evidence)
      )
    )
    assertEquals(output.rows.size, 480)
    assertEquals(output.summary.contrasts, QueryContrasts(480, 14, 9, 3, 454))
    output.rows.foreach { row =>
      row.status match
        case QueryStatus.Contributing(ms, bs, ds) =>
          val stored = expected(row.query.participant -> row.query.trial)
          val key    = prepared.admitted.input.trials.rows
            .find(t =>
              t.key.participant == row.query.participant && t.key.trial == row.query.trial && t.key.phase == row.query.phase.label
            )
            .get
            .key
          Vector("0.5", "1", "2", "4").zipWithIndex.foreach { (sigma, scale) =>
            val directRows = direct.scales(scale).contrast match
              case eyes4s.plan.ScaleContrast.Rows(rows) => rows
              case other => fail(s"Direct contrast missing: $other")
            val exact = get(
              directRows.get(ResultRef.ContrastRow(scale, key)).toRight("direct D missing")
            ).outcome.toOption.get.value.value
            assertEquals(ds(scale), exact)
            Vector("M" -> ms(scale), "B" -> bs(scale), "D" -> ds(scale)).foreach {
              (role, value) =>
                val pinned = get(stored.downField("scales").downField(sigma).get[Double](role))
                assert(
                  math.abs(value - pinned) <= ScoresTolerance,
                  s"${row.query.label}/$sigma/$role: $value vs $pinned"
                )
            }
          }
        case status if status.isFailed =>
          assert(
            get(expected(row.query.participant -> row.query.trial).get[String]("status"))
              .startsWith("failed:")
          )
        case QueryStatus.NoMatch(_) =>
          assertEquals(
            get(expected(row.query.participant -> row.query.trial).get[String]("status")),
            "no-match"
          )
        case QueryStatus.NotAdmitted(_) =>
          assert(!expected.contains(row.query.participant -> row.query.trial))
        case other => fail(s"Unexpected native status $other")
    }
