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

package example

import cats.data.NonEmptyVector
import eyes4s.codec.*
import eyes4s.codec.CodecDiagnostics.given
import eyes4s.core.FixationBoundary
import eyes4s.design.*
import eyes4s.fs2.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.laws.Tolerance
import eyes4s.plan.*
import io.circe.Json

/** UI-G1, the temporal route: the pinned temporal table in; recipe discovery,
  * preflight with its coverage warnings, a run through `TemporalExecution`
  * with progress and cancellation, inspection of every cell back to the
  * table's records, save under one manifest with the temporal roles, reload
  * through a fresh resolver and a rerun bit for bit, on the JVM and Scala.js.
  * Every test runs for the shipped cosine and for the consumer's scaled
  * cosine with its own key and score types. The oracles are the independent
  * integer-overlap ledgers and 60-digit cosine targets of
  * `tools/r-parity/fixtures/temporal.json`.
  */
class TemporalJourneySuite extends munit.CatsEffectSuite:
  import JourneySetup.{comparison, pairs, quanta}

  private val Oracle = Tolerance(absolute = 1e-12, relative = 0)

  private def get[E, A](value: Either[E, A]): A  = value.fold(e => fail(s"$e"), identity)
  private def named(value: String): ArtifactName = get(ArtifactName.of(value))

  /** Logical CSV records (the header is record 1) of one trial's rows in the
    * temporal table, read from the text rather than from any eyes4s value.
    */
  private def records(participant: String, image: String, phase: String): Vector[Int] =
    TemporalConsumerFixtures.csv.linesIterator.zipWithIndex.collect {
      case (line, index)
          if line.split(',').take(3).toVector == Vector(participant, image, phase) =>
        index + 1
    }.toVector

  private def refused(reloaded: Either[JourneyError, ?]): NonEmptyVector[ResolveError] =
    reloaded match
      case Left(JourneyError.Resolve(errors)) => errors
      case other                              => fail(s"expected a refusal: $other")

  private def journey[K, P, S, D](t: TemporalCase[K, P, S, D]): Unit =
    import t.{c, route}
    val name = s"temporal ${c.name}"

    test(s"$name: the temporal table imports with a measured epoch for every trial") {
      assertEquals(t.imported.rejected, Vector.empty)
      assertEquals(t.ledger.outcome, AdmissionOutcome.Complete)
      assertEquals(t.ledger.records.map(_.record), (2 to 73).toVector)
      assertEquals(t.base.trials.rows.size, 18)
      assertEquals(t.input.study.reference, t.base.reference)
      assertEquals(t.input.epochs.size, 18)
      t.base.trials.rows.foreach { trial =>
        val epoch = get(t.input.epochs.get(trial.key).toRight(s"no epoch for ${trial.key}"))
        assertEquals(epoch.anchor.toMicros, 0L)
        assertEquals(epoch.coverage.clock, trial.value.clock)
        assertEquals(
          epoch.coverage.intervals.map(i => (i.onset.toMicros, i.offset.toMicros)),
          TemporalConsumerFixtures.coverage(c.label(trial.key))
        )
      }
      // A second epoch for one trial is the constructor's refusal, naming the
      // trial by its key digest under the route's own layout.
      val stray = t.epochs().head
      assertEquals(
        route.input(t.base, t.epochs() :+ stray).map(_.reference),
        Left(
          TemporalStudyError.DuplicateEpochs(
            Vector(route.study.layout.digest.digest(stray._1).render)
          )
        )
      )
    }

    test(s"$name: the recipe is discovered with its windows, repetitions and capability") {
      assert(Preflight.families.contains(RecipeFamily.TemporalStudy))
      val inspection = get(t.study.inspect)
      assertEquals(inspection.description, t.study.description)
      val base = get(t.study.base.inspect)
      assertEquals(
        inspection.fields.map(_.info.id),
        base.fields.map(_.info.id) ++ Vector(
          "temporal.input",
          "temporal.boundary",
          "temporal.scope"
        ) ++ (0 until 4).map(i => s"window.$i") ++ (0 until 2).map(i => s"repetition.$i")
      )
      assertEquals(
        inspection.fields.find(_.info.id == "window.3").map(_.values),
        Some(
          Vector(
            Provenance.Param.Text("outside"),
            Provenance.Param.Text("950000"),
            Provenance.Param.Text("1100000")
          )
        )
      )
      assertEquals(base.execution, ExecutionCapability.BoundedComparison)
      // The temporal runner steps each cell's study cursor in comparison quanta.
      assertEquals(inspection.execution, base.execution)
    }

    test(s"$name: each repetition's pair design previews exactly the oracle's pairs") {
      // The pairs every window of a repetition compares, paged from the prepared
      // schedule without scoring, against the independent pair table.
      val quantum = get(PairQuantum.of(4))
      def page(
          cursor: PairCursor[K, K],
          seen: Vector[ScheduledPair[K, K]]
      ): Vector[ScheduledPair[K, K]] =
        get(cursor.advance(quantum)) match
          case PairPage.More(found, _, next) => page(next, seen ++ found)
          case PairPage.Done(found, _, _)    => seen ++ found
      val previewed = t.work.repetitions.flatMap { repetition =>
        val preview = get(repetition.prepared.preview)
        // The third phase takes no part in this repetition: named, not dropped.
        val phases = Set(repetition.contrast.focalPhase, repetition.contrast.referencePhase)
        assertEquals(
          preview.excludedPhases,
          t.base.trials.rows.map(_.key).filterNot(k => phases(c.label(k).split('/')(2)))
        )
        Vector(preview.matched -> "matched", preview.controls -> "control").flatMap {
          (schedule, kind) =>
            page(schedule.start, Vector.empty).map(p =>
              (repetition.contrast.name, c.label(p.left), c.label(p.right), kind)
            )
        }
      }
      assertEquals(previewed.sorted, TemporalConsumerFixtures.pairs.sorted)
      assertEquals(previewed.size, 36)
    }

    test(
      s"$name: preflight warns of windows without observed coverage and refuses stale work"
    ) {
      val absent = t.study.preflight(None, pairs)
      assertEquals(absent.findings, Vector(TemporalFinding.MissingArtifact(t.study.input)))
      assertEquals(absent.availability, Availability.Unavailable)
      val report = t.study.preflight(Some(t.input), pairs)
      val keys   = t.base.trials.rows.map(_.key)
      assertEquals(report.findings, keys.map(TemporalFinding.NoObservedCoverage(_, "outside")))
      assertEquals(report.availability, Availability.Ready)
      assertEquals(
        report.findings.map(f => (f.severity, f.category, f.remedy)).distinct,
        Vector((Severity.Warning, FindingClass.DataDependent, Remedy.AcceptMissingObservation))
      )
      assertEquals(
        report.findings.map(Diagnostic.of(_).code.render).distinct,
        Vector("temporal-finding.no-observed-coverage")
      )
      assertEquals(report.notChecked, Preflight.temporalUnchecked)
      assertEquals(report.confirm(t.study, t.input), Right(()))
      // Revised after preflight: the report names the changed field.
      val revised = t.plan(t.input, bound = FixationBoundary.FullyContained)
      assertEquals(t.study.diff(revised).map(_.field), Vector("temporal.boundary"))
      assertEquals(
        report.confirm(revised, t.input),
        Left(PreflightError.ChangedPlan(RecipeFamily.TemporalStudy, t.study.diff(revised)))
      )
      // An input without one trial's epoch is another input, and a warning of its own.
      val focal   = c.key("s1", "b", "recall")
      val partial = get(route.input(t.base, t.epochs(Set(c.label(focal)))))
      assertEquals(
        report.confirm(t.study, partial),
        Left(
          PreflightError.ChangedInput(
            RecipeFamily.TemporalStudy,
            t.input.reference,
            partial.reference
          )
        )
      )
      val replanned = t.plan(partial)
      val warned    = replanned.preflight(Some(partial), pairs)
      assertEquals(warned.availability, Availability.Ready)
      assertEquals(
        warned.findings.collect { case f @ TemporalFinding.MissingEpoch(_) => f },
        Vector(TemporalFinding.MissingEpoch(focal))
      )
      assertEquals(
        Diagnostic.of(TemporalFinding.MissingEpoch(focal)).code.render,
        "temporal-finding.missing-epoch"
      )
    }

    test(
      s"$name: a run reports progress cell by cell with stated totals and ends in the pure result"
    ) {
      t.events.map { events =>
        val progress = events.collect { case RunEvent.Advanced(p) => p }
        assert(progress.forall(_.run == t.id))
        assertEquals(progress.map(_.step), (1L to progress.size.toLong).toVector)
        val blocks = progress.foldLeft(Vector.empty[Vector[TemporalProgress]]) { (acc, p) =>
          if acc.lastOption.exists(_.head.segment == p.segment) then acc.init :+ (acc.last :+ p)
          else acc :+ Vector(p)
        }
        val order = for
          r       <- 0 until 2
          w       <- 0 until 4
          segment <- TemporalSegment.Preparing(r, w) +: (0 until 2).toVector.flatMap { s =>
            Vector(
              StudySegment.Estimating(s),
              StudySegment.Comparing(s, StudyDesign.Matched),
              StudySegment.Reducing(s, StudyDesign.Matched),
              StudySegment.Comparing(s, StudyDesign.Control),
              StudySegment.Reducing(s, StudyDesign.Control),
              StudySegment.Contrasting(s)
            ).map(TemporalSegment.Studying(r, w, _))
          }
        yield segment
        assertEquals(blocks.map(_.head.segment), order.toVector)
        assert(blocks.forall(b => b.map(_.segmentTotal).distinct.size == 1))
        // Preparation: one unit per trial of every cell, exactly.
        assertEquals(
          blocks
            .flatMap(b =>
              b.head.segment match
                case TemporalSegment.Preparing(_, _) =>
                  Vector(b.head.segmentTotal -> b.last.segmentUnits)
                case TemporalSegment.Studying(_, _, _) => Vector.empty
            )
            .distinct,
          Vector(SegmentTotal.Exact(18) -> 18L)
        )
        events.collect { case RunEvent.Finished(o) => o } match
          case Vector(RunOutcome.Completed(id, last, result)) =>
            assertEquals((id, last), (t.id, progress.last))
            assert(t.same(result, t.pure), "the streamed result is the pure run")
          case other => fail(s"expected one completed outcome: $other")
      }
    }

    test(s"$name: the result agrees with the integer-overlap ledgers and the decimal targets") {
      val result = t.pure
      assertEquals(
        result.cells.map(c => c.repetition.name -> c.window.name),
        Vector(
          "recall-encode" -> "early",
          "recall-encode" -> "middle",
          "recall-encode" -> "late",
          "recall-encode" -> "outside",
          "retest-recall" -> "early",
          "retest-recall" -> "middle",
          "retest-recall" -> "late",
          "retest-recall" -> "outside"
        )
      )
      // Every cell holds every trial's occupancy; each equals the oracle's ledger exactly.
      val oracle = TemporalConsumerFixtures.ledgers.map {
        (key, window, retained, observed, missing) =>
          (key, window) -> (retained, observed, missing)
      }.toMap
      val found = t.ledgers(result)
      assertEquals(found.size, 2 * 72)
      found.foreach { (at, ledger) => assertEquals(ledger, oracle(at), at) }
      assertEquals(found.map(_._1).toSet, oracle.keySet)
      // All 96 contrasts: within the named tolerance, or failed where the oracle has none.
      val m        = c.multiplier
      val computed = t.contrasts(result)
      assertEquals(computed.size, TemporalConsumerFixtures.targets.size)
      TemporalConsumerFixtures.targets.foreach { target =>
        val at = (target.repetition, target.key, target.window, target.sigma)
        (computed(at), target.difference) match
          case (Some(value), Some(expected)) =>
            assert(
              Oracle.approxEquals(value, m * expected),
              s"$at: $value versus ${m * expected}"
            )
          case (value, expected) => assertEquals(value, expected, at)
      }
      assertEquals(
        TemporalConsumerFixtures.targets.count(_.difference.isEmpty),
        computed.count(_._2.isEmpty)
      )
    }

    test(s"$name: an empty window fails every pair of its cells, as located evidence") {
      val result  = t.pure
      val sources = get(StudySources.of(t.base, t.ledger)(using route.study.layout.digest))
      val view    = get(ResultInspection.temporal(result, sources, t.schema))
      assertEquals(view.cellNames, result.cells.map(c => c.repetition.name -> c.window.name))
      val outside   = get(view.cell("recall-encode", "outside"))
      val occupancy = outside.occupancy.first(get(PageSize.of(PageSize.maximum))).entries
      assertEquals(occupancy.size, 18)
      assert(
        occupancy.forall(
          _.outcome.exists(o => o.observedMicros == 0L && o.missingMicros == 150000L)
        ),
        "the outside window is observed nowhere"
      )
      // Per scale: 18 trials, 6 matched and 12 control pairs, 6 + 6 reductions
      // and 6 contrast rows, every one a located failure. The binned scale
      // fails at the zero-mass occupancy, the Gaussian one at estimation.
      val failures = outside.study.failures
      assertEquals(failures.size, 2 * (18 + 6 + 6 + 12 + 6 + 6))
      assert(
        failures.forall(d =>
          d.subject.take(3) == Vector(
            Locus.Repetition("recall-encode"),
            Locus.Window("outside"),
            d.subject(2)
          ) && (d.subject(2) == Locus.Scale(0) || d.subject(2) == Locus.Scale(1))
        ),
        "every failure names its cell and scale"
      )
      assertEquals(
        failures.map(d => (d.subject(2), d.code.render, d.causes.map(_.code.render))).distinct,
        Vector(
          (Locus.Scale(0), "study-failure.occupancy", Vector("surface.degenerate-total")),
          (Locus.Scale(0), "reduction.failed-scores", Vector.empty),
          (
            Locus.Scale(0),
            "contrast-row.reduction-failures",
            Vector("reduction.failed-scores", "reduction.failed-scores")
          ),
          (Locus.Scale(1), "study-failure.estimation", Vector("estimate.no-mass")),
          (Locus.Scale(1), "reduction.failed-scores", Vector.empty),
          (
            Locus.Scale(1),
            "contrast-row.reduction-failures",
            Vector("reduction.failed-scores", "reduction.failed-scores")
          )
        )
      )
      // Each estimation failure links to the records of its trial's rows.
      val first = get(
        failures
          .find(_.code.render == "study-failure.estimation")
          .toRight("no estimation failure")
      )
      assertEquals(first.keys, Vector(c.key("s1", "a", "encode")))
      assertEquals(
        first.sources,
        records("s1", "a", "encode").map(SourceLink.Record(t.ledger.source, _))
      )
      // The observed windows are complete: no failure in early, middle or late.
      assertEquals(
        view.cellNames
          .filter(_._2 != "outside")
          .flatMap((r, w) => get(view.cell(r, w)).study.failures),
        Vector.empty
      )
      // A trial without an epoch fails as a typed temporal failure in every cell.
      val focal   = c.key("s1", "b", "recall")
      val partial = get(route.input(t.base, t.epochs(Set(c.label(focal)))))
      val missing = get(t.prepare(t.plan(partial), partial).run)
      missing.cells.foreach { cell =>
        assertEquals(
          cell.occupancy.collect { case (k, Left(e)) =>
            k -> Diagnostic.of(e).code.render
          },
          Vector(focal -> "temporal.missing-epoch")
        )
      }
    }

    test(s"$name: a table on another display fails every trial by its frame in every cell") {
      // The temporal table read on a display named like the plan's, with other bounds.
      val wide  = get(eyes4s.kernel.Frame.screen("display", 4, 4))
      val moved = get(
        get(
          eyes4s.io.FixationCsv.read(
            TemporalConsumerFixtures.csv,
            JourneySetup.columns,
            route.study.reader,
            wide,
            eyes4s.io.TimestampUnit.Microseconds
          )
        ).requireComplete
      )
      val epochs = moved.trials.rows.map { trial =>
        trial.key -> get(t.input.epochs.get(trial.key).toRight("no epoch"))
      }
      val input  = get(route.input(moved, epochs))
      val study  = t.plan(input, scales = t.estimates.take(1))
      val report = study.preflight(Some(input), pairs)
      val keys   = moved.trials.rows.map(_.key)
      assertEquals(
        report.findings.map(f => Diagnostic.of(f).code.render).distinct,
        Vector("temporal-finding.study", "temporal-finding.no-observed-coverage")
      )
      assertEquals(
        report.findings.collect { case TemporalFinding.Study(f) =>
          f.keys -> Diagnostic.of(f).code.render
        },
        keys.map(k => Vector(k) -> "study-finding.frame-mismatch")
      )
      assertEquals(report.availability, Availability.Ready)
      val work = t.prepare(study, input)
      t.runner.start(work, comparison, quanta).use(_.outcome).map { outcome =>
        val result = t.completed(outcome)
        assertEquals(result.cells.size, 8)
        result.cells.foreach { cell =>
          assertEquals(
            cell.result.scales.head.estimation
              .map((k, e) => k -> e.left.map(Diagnostic.of(_).code.render)),
            keys.map(k => k -> Left("study-failure.frame")),
            s"${cell.repetition.name}/${cell.window.name}"
          )
        }
      }
    }

    test(s"$name: cancelling lands between steps inside a cell and leaves no result") {
      for
        events <- t.events
        progress = events.collect { case RunEvent.Advanced(p) => p }
        // Inside the second cell's matched comparison.
        mid = progress.indexWhere(p =>
          p.segment == TemporalSegment.Studying(
            0,
            1,
            StudySegment.Comparing(0, StudyDesign.Matched)
          )
        ) + 2
        cancelled   <- t.cancelAfter(mid)
        immediately <- t.cancelAfter(0)
        outcome     <- t.runner.start(t.work, comparison, quanta).use(_.outcome)
      yield
        val (stopped, observed) = cancelled
        val cell                =
          TemporalSegment.Studying(0, 1, StudySegment.Comparing(0, StudyDesign.Matched))
        assertEquals((progress(mid - 1).segment, progress(mid).segment), (cell, cell))
        assertEquals(stopped, RunOutcome.Cancelled(t.id, Some(progress(mid - 1))): t.Outcome)
        assertEquals(observed, Vector(progress(mid - 1)))
        assertEquals(
          immediately,
          (RunOutcome.Cancelled(t.id, None): t.Outcome) -> Vector.empty[TemporalProgress]
        )
        assert(t.same(t.completed(outcome), t.pure), "a run after a cancellation completes")
    }

    test(s"$name: a saved run reloads through a fresh resolver and reruns bit for bit") {
      for
        (result, saved) <- t.saved
        files    = t.storage(saved)
        reloaded = get(t.reload(files))
        rerun <- TemporalJourney.rerun(reloaded, pairs, comparison, quanta)
      yield
        assertEquals(
          saved.manifest.entries.map(e => e.name.value -> e.role),
          Vector(
            TemporalJourney.baseEntry   -> ArtifactRole.StudyInput,
            TemporalJourney.ledgerEntry -> ArtifactRole.AdmissionLedger,
            TemporalJourney.inputEntry  -> ArtifactRole.TemporalInput,
            TemporalJourney.planEntry   -> ArtifactRole.TemporalPlan,
            TemporalJourney.resultEntry -> ArtifactRole.TemporalResult
          )
        )
        assertEquals(
          saved.manifest.relations.map(_.kind),
          Vector("ledger-of", "temporal-base", "temporal-plan-input", "temporal-result-of")
        )
        assertEquals(reloaded.base.reference, t.base.reference)
        assertEquals(reloaded.ledger, t.ledger)
        assertEquals(reloaded.input.reference, t.input.reference)
        // Every epoch exactly: anchor, clock and observed intervals in microseconds.
        def epochs(input: eyes4s.plan.TemporalStudyInput[K, Px]) =
          input.study.trials.rows.map(_.key).map { key =>
            key -> input.epochs
              .get(key)
              .map(e =>
                (
                  e.anchor.toMicros,
                  e.coverage.clock,
                  e.coverage.intervals.map(i => i.onset.toMicros -> i.offset.toMicros)
                )
              )
          }
        assertEquals(epochs(reloaded.input), epochs(t.input))
        assertEquals(reloaded.plan.diff(t.study), Vector.empty)
        assert(t.same(reloaded.result, result), "the archive decodes to the result")
        val again = t.completed(get(rerun))
        assert(t.same(again, result), "the rerun reproduces every double and failure")
        val stored = get(
          saved.artifacts.find(_.name.value == TemporalJourney.resultEntry).toRight("no result")
        )
        assertEquals(t.archived(again), stored.entry.sha256)
    }

    test(s"$name: a changed, missing or replaced artifact is refused by name, with a code") {
      t.saved.map { (_, saved) =>
        val files  = t.storage(saved)
        val result = TemporalJourney.resultEntry
        val digest = refused(t.reload(files.updated(result, Forgery.flip(files(result), 40))))
        assertEquals(
          digest,
          NonEmptyVector.one(
            ResolveError.Digest(
              named(result),
              get(saved.manifest.entry(named(result)).toRight("no entry")).sha256,
              ByteDigest.sha256(Forgery.flip(files(result), 40))
            )
          )
        )
        assertEquals(Diagnostic.of(digest.head).code.render, "resolve.digest")
        val missing = refused(t.reload(files - TemporalJourney.ledgerEntry))
        assertEquals(
          missing,
          NonEmptyVector.one(ResolveError.Missing(named(TemporalJourney.ledgerEntry)))
        )
        // The temporal input replaced by one without an epoch, re-declared consistently.
        val focal   = c.key("s1", "b", "recall")
        val partial = get(route.input(t.base, t.epochs(Set(c.label(focal)))))
        val swapped =
          get(StoredArtifact.temporalInput(TemporalJourney.inputEntry, route.inputs, partial))
        val replaced = refused(
          t.reload(Forgery.redeclare(files, saved, TemporalJourney.inputEntry, swapped.bytes))
        )
        assertEquals(
          replaced,
          NonEmptyVector.one(
            ResolveError.Identity(
              named(TemporalJourney.inputEntry),
              t.input.reference.digest,
              partial.reference.digest
            )
          )
        )
        assertEquals(Diagnostic.of(replaced.head).code.render, "resolve.identity")
      }
    }

    test(s"$name: an archive naming an unregistered temporal method is refused by method") {
      t.saved.map { (_, saved) =>
        val files  = t.storage(saved)
        val source = SavedRun.source(files.get)
        val method = route.study.method.id
        // Only the fixation registrations: both temporal entries are unsupported schemas.
        val fixation = get(route.study.decoders)
        assertEquals(
          ArtifactResolver.resolve(saved.address, source, fixation).left.map(_.toVector),
          Left(
            Vector(
              ResolveError.Decode(
                named(TemporalJourney.planEntry),
                CodecError
                  .UnsupportedSchema("temporal-plan", route.persistence.schema, Vector.empty)
              ),
              ResolveError.Decode(
                named(TemporalJourney.resultEntry),
                CodecError.UnsupportedSchema(
                  "temporal-result",
                  DefinitionId.temporalResult,
                  Vector.empty
                )
              )
            )
          )
        )
        // Temporal registries that know no method: refused by the method the archive names.
        val empty =
          fixation.withTemporal(
            TemporalRegistry.empty[K, Px],
            TemporalResultRegistry.empty[K, Px]
          )
        val unknown =
          ArtifactResolver.resolve(saved.address, source, empty).left.map(_.toVector)
        assertEquals(
          unknown,
          Left(
            Vector(
              ResolveError
                .Decode(named(TemporalJourney.planEntry), CodecError.MissingMethod(method)),
              ResolveError.Decode(
                named(TemporalJourney.resultEntry),
                CodecError.MissingResultCodec(method)
              )
            )
          )
        )
        assertEquals(
          get(unknown.swap).map(e => Diagnostic.of(e).causes.map(_.code.render)),
          Vector(Vector("codec.missing-method"), Vector("codec.missing-result-codec"))
        )
      }
    }

    test(s"$name: emit portable temporal evidence for the JVM and Scala.js comparison") {
      for
        events          <- t.events
        (result, saved) <- t.saved
        cancelled       <- t.cancelAfter(40)
      yield
        val progress  = events.collect { case RunEvent.Advanced(p) => p }
        val reloaded  = get(t.reload(t.storage(saved)))
        val runtime   = if System.getProperty("java.vm.name") == "Scala.js" then "js" else "jvm"
        val contrasts = t.contrasts(reloaded.result).toVector.sortBy(_._1.toString).map {
          case ((repetition, key, window, sigma), value) =>
            Json.obj(
              "repetition" -> Json.fromString(repetition),
              "key"        -> Json.fromString(key),
              "window"     -> Json.fromString(window),
              "sigma"      -> sigma.fold(Json.Null)(Json.fromDoubleOrNull),
              "value"      -> value.fold(Json.Null)(Json.fromDoubleOrNull),
              "bits"       -> value.fold(Json.Null)(v =>
                Json.fromString(
                  java.lang.Long.toHexString(java.lang.Double.doubleToRawLongBits(v))
                )
              )
            )
        }
        val ledgers = t.ledgers(reloaded.result).distinct.map {
          case ((key, window), (retained, observed, missing)) =>
            Json.obj(
              "key"      -> Json.fromString(key),
              "window"   -> Json.fromString(window),
              "retained" -> Json.arr(retained.map(Json.fromLong)*),
              "observed" -> Json.fromLong(observed),
              "missing"  -> Json.fromLong(missing)
            )
        }
        val segments =
          progress.groupBy(_.segment).toVector.sortBy(_._2.head.step).map { (segment, steps) =>
            Json.obj(
              "segment" -> Json.fromString(segment.toString),
              "total"   -> Json.fromString(steps.head.segmentTotal.toString),
              "units"   -> Json.fromLong(steps.last.segmentUnits),
              "steps"   -> Json.fromInt(steps.size)
            )
          }
        println(
          "EYES4S_TEMPORAL_JOURNEY=" + Json
            .obj(
              "runtime"    -> Json.fromString(runtime),
              "route"      -> Json.fromString(c.name),
              "multiplier" -> Json.fromDoubleOrNull(c.multiplier),
              "base"       -> Json.fromString(reloaded.base.reference.digest),
              "input"      -> Json.fromString(reloaded.input.reference.digest),
              "plan"       -> get(route.persistence.codec.encode(reloaded.plan)),
              "contrasts"  -> Json.arr(contrasts*),
              "ledgers"    -> Json.arr(ledgers*),
              "segments"   -> Json.arr(segments*),
              "cancelled"  -> cancelled._1.progress.fold(Json.Null)(p =>
                Json.obj(
                  "step"    -> Json.fromLong(p.step),
                  "segment" -> Json.fromString(p.segment.toString)
                )
              ),
              "fingerprint_size" -> Json.fromInt(t.bits(result).size),
              "archive_sha256"   -> Json.fromString(t.archived(result).hex)
            )
            .noSpaces
        )
    }

  journey(TemporalRun.cosine)
  journey(TemporalRun.scaled)

  test("temporal: the extension's binned contrasts are exactly twice the shipped cosine's") {
    val plain = TemporalRun.cosine.contrasts(TemporalRun.cosine.pure)
    val twice = TemporalRun.scaled.contrasts(TemporalRun.scaled.pure)
    assertEquals(twice.keySet, plain.keySet)
    plain.foreach { case (at @ (_, _, _, sigma), value) =>
      if sigma.isEmpty then
        assertEquals(
          twice(at).map(java.lang.Double.doubleToRawLongBits),
          value.map(v => java.lang.Double.doubleToRawLongBits(2 * v)),
          at
        )
    }
  }
