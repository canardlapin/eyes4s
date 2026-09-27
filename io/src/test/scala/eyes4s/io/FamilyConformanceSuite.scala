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

package eyes4s.io

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO, Ref}
import eyes4s.codec.*
import eyes4s.compare.*
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.detect.*
import eyes4s.fs2.{Execution, RecordingExecution, RunOutcome, StudyExecution, Submission}
import eyes4s.fs2.TemporalExecution
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.{Deg, Px}
import eyes4s.plan.*
import eyes4s.surface.EdgePolicy
import io.circe.Json
import scala.compiletime.summonAll
import scala.compiletime.testing.typeCheckErrors
import scala.deriving.Mirror

/** Every recipe family meets the Studio definition of done, checked through
  * one witness per family:
  *
  *   - inspect explains every field of the plan's description;
  *   - its form is complete: every form field parses the fixture's raw value
  *     and writes it back unchanged, every view is well formed, and the
  *     form's values rebuild the fixture plan's description;
  *   - preflight reports the family's own `RecipeFamily`, is ready on the
  *     fixture and blocked without its input, and every diagnostic it projects
  *     (causes included) carries a catalogued code;
  *   - `Stepwise.complete` at the finest and the default quanta equals `run`,
  *     and the finest cut takes more steps than the default (the cursor
  *     honours its quanta);
  *   - the shared `Execution` runner completes with `run`'s result and, when
  *     cancelled while parked between two steps, settles `Cancelled` after
  *     exactly the steps the cursor took;
  *   - the plan and result codecs round-trip, re-encoding to the same bytes;
  *   - the result projects to a `ResultTable`.
  *
  * Results are compared through their archives: two results are the same
  * when the family's result codec writes the same document for both.
  *
  * A witness is a `given FamilyWitness[F]` for each case type `F` of
  * `RecipeFamily`, collected by [[FamilyWitnesses.all]] through the enum's
  * mirror, so a family added without a witness is a compile error on every
  * build, not only under `-Werror`; the suite proves it with `typeCheckErrors`.
  * An obligation a family cannot meet yet is a named [[FamilyWitness.Gap]]
  * and the pinned set of gaps must shrink, never grow silently.
  *
  * ==Mutation receipts==
  *
  * Each mutant was applied to the library, this suite run on the JVM, and the
  * change reverted; every one was killed by the named obligations. Deleting
  * the temporal witness fails compilation with "No given instance of type
  * FamilyWitness[TemporalStudy]".
  *
  * {{{
  * | mutant                                                  | killed by                           |
  * |---------------------------------------------------------|-------------------------------------|
  * | StudyReport.family reports TemporalStudy                | FixationStudy preflight             |
  * | the recording Stepwise instance ignores its quanta      | EventRecording stepwise             |
  * | MachineCursor resets its machine state at each chunk    | EventRecording stepwise, execution  |
  * | ReductionCursor skips a contribution at a page boundary | FixationStudy, TemporalStudy        |
  * |                                                         | stepwise and execution completion   |
  * | temporal inspect metadata loses the boundary field      | TemporalStudy inspect               |
  * | TemporalStudyCodec reads back only the first window     | TemporalStudy archive               |
  * | ResultExports tables lose every row                     | FixationStudy, TemporalStudy table  |
  * | ContrastCsv drops the last contrast row of each scale    | FixationStudy, TemporalStudy table  |
  * | ContrastCsv writes negated differences                  | FixationStudy, TemporalStudy table  |
  * | TemporalContrastCsv drops a trial's coverage row        | TemporalStudy table                 |
  * | the runner numbers the first step 2                     | every family's execution checks     |
  * | the runner's between-step hook is uncancelable          | every family's cancellation check   |
  * }}}
  *
  * The table obligation compares each table with what the result implies: the
  * contrast table's row count (one row per failed scale, else one per contrast
  * row) and its `difference` column in row order, and the coverage table's
  * row count (one per trial and cell). `ResultExports.contrasts` is not a
  * projection of any family's result (it takes `CompareError` sources, a study
  * result retains `StudyFailure` ones), so a mutant there is
  * `BaselineExportsSuite`'s to kill, not this suite's.
  */
class FamilyConformanceSuite extends munit.FunSuite:
  import FamilyWitness.*

  private val witnesses: Vector[FamilyWitness[?]] =
    import FamilyWitnesses.given
    FamilyWitnesses.all

  test("every recipe family has exactly one witness, in declaration order") {
    assertEquals(witnesses.map(_.family), RecipeFamily.values.toVector)
  }

  test("a recipe family without a witness does not compile") {
    assertEquals(
      typeCheckErrors("""
        import FamilyWitnesses.given
        FamilyWitnesses.all
      """),
      Nil
    )
    val missing = typeCheckErrors("""
      import FamilyWitnesses.{study, recording}
      FamilyWitnesses.all
    """)
    assert(missing.nonEmpty, "a missing witness compiled")
    assert(missing.exists(_.message.contains("TemporalStudy")), missing.map(_.message))
  }

  test("every analysis kind maps to its family, and the unmapped kinds are the allowlist") {
    assertEquals(
      AnalysisKind.values.toVector.filter(_.family.isEmpty),
      AnalysisKind.withoutFamily
    )
    assertEquals(
      AnalysisKind.withoutFamily.toSet,
      Set(
        AnalysisKind.Repetition,
        AnalysisKind.PointSampling,
        AnalysisKind.TemplateFit,
        AnalysisKind.Decomposition,
        AnalysisKind.ScanpathComparison
      )
    )
    RecipeFamily.values.foreach { family =>
      assertEquals(
        AnalysisKind.values.toVector.filter(_.family.contains(family)),
        Vector(
          AnalysisKind.of(family)
        )
      )
    }
    assertEquals(AnalysisComponent.values.toVector, Vector(AnalysisComponent.Epochs))
  }

  test("the named gaps are exactly the pinned ones") {
    assertEquals(
      witnesses.flatMap(w => w.gaps.map(g => w.family -> g.obligation)).toSet,
      Set(RecipeFamily.EventRecording -> Obligation.Table)
    )
    witnesses.flatMap(_.gaps).foreach(gap => assert(gap.reason.nonEmpty, gap))
  }

  witnesses.foreach { w =>
    val name = w.family.toString

    test(s"$name: inspect explains every field") {
      w.inspect()
    }

    test(s"$name: its form is complete") {
      w.formComplete()
    }

    test(s"$name: preflight names its family and projects catalogued diagnostics") {
      w.preflight()
    }

    test(s"$name: Stepwise.complete equals run") {
      w.stepwise()
    }

    test(s"$name: Execution completes with run's result") {
      w.completes().unsafeToFuture()
    }

    test(s"$name: Execution cancels between steps") {
      w.cancels().unsafeToFuture()
    }

    test(s"$name: the plan and result archives round-trip") {
      w.archive()
    }

    test(s"$name: the result projects to a table, or the gap is named") {
      w.table() match
        case Right(tables) =>
          assert(tables.nonEmpty)
          tables.foreach { t =>
            val clue = t.table.family
            assertEquals(t.table.rows.size, t.rows, clue)
            t.differences.foreach { expected =>
              val column = t.table.columns.indexWhere(_.name == "difference")
              assert(column >= 0, clue)
              val written = t.table.rows.collect(_(column) match
                case ResultCell.Number(v) => v)
              assertEquals(written, expected, clue)
              // Not vacuous: the fixture has differences a sign flip would change.
              assert(expected.exists(_ != 0.0), s"$clue differences $expected")
            }
          }
        case Left(gap) => assertEquals(gap.obligation, Obligation.Table)
    }
  }

/** One family's evidence for the obligations. Type members fix the family's
  * cursor, stage, segment, error and result, so the checks are written once.
  */
abstract class FamilyWitness[F](val family: F & RecipeFamily):
  import FamilyWitness.*

  type Id
  type Cursor
  type Stage
  type Segment
  type Error
  type Result

  def description: Vector[(String, Vector[Provenance.Param])]
  def inspection: Either[DescriptorError, RecipeInspection]

  /** Preflight with the fixture's input, and without any input. */
  def ready: Preflighted
  def unavailable: Preflighted

  def run: Either[Error, Result]
  def cursor: Either[Error, Cursor]
  def submission(quanta: WorkQuanta): Submission[Id, Cursor, Stage, Segment, Error, Result]
  given stepping: Stepwise[Cursor, Stage, Error, Result]

  /** The result as its archive writes it, and that document decoded and re-encoded. */
  def archived(result: Result): Either[CodecError, Json]
  def rearchived(document: Json): Either[CodecError, Json]

  /** The plan as its codec writes it; the decoded plan's description and re-encoding. */
  def savedPlan: Either[CodecError, Json]
  def reloadedPlan(
      document: Json
  ): Either[CodecError, (Vector[(String, Vector[Provenance.Param])], Json)]

  /** The result's tables, each with the row count and differences the result implies. */
  def tables(result: Result): Either[Gap, Either[ResultExportError, Vector[Tabulated]]]

  /** The family's form on the fixture plan. */
  def form: FormEvidence

  final def gaps: Vector[Gap] =
    run.toOption.toVector.flatMap(r => tables(r).left.toOption)

  private def get[E, A](e: Either[E, A]): A =
    e.fold(error => throw new AssertionError(s"$family: $error"), identity)
  private def check(condition: Boolean, clue: => Any): Unit =
    if !condition then throw new AssertionError(s"$family: $clue")

  final def inspect(): Unit =
    val inspected = get(inspection)
    check(inspected.description == description, inspected.description)
    check(inspected.fields.forall(_.info.meaning.nonEmpty), "a field has no meaning")

  /** Every field of the family's form parses the fixture's raw value on its
    * own and writes it back unchanged, every view (parts included) is well
    * formed, the fixture has no value the form lacks a field for, and a form
    * that builds a whole plan rebuilds the fixture's description.
    */
  final def formComplete(): Unit =
    val FormEvidence(fields, values, rebuilt, covers, context) = form
    check(fields.nonEmpty, "the form has no fields")
    fields.foreach { f =>
      val raw = values.get(f.view.id)
      check(f.restore(raw) == Right(raw), s"${f.view.id}: ${f.restore(raw)}")
    }
    def parts(v: FieldView): Vector[FieldView] = v +: (v.kind match
      case FieldKind.Group(ps, _)       => ps.flatMap(parts)
      case FieldKind.Variant(cases)     => cases.flatMap(_.parts).flatMap(parts)
      case FieldKind.Optional(of, _)    => parts(of)
      case FieldKind.Repeated(of, _, _) => parts(of)
      case _                            => Vector.empty)
    fields.flatMap(f => parts(f.view)).foreach { v =>
      check(FieldView.of(v.id, v.version, v.meaning, v.kind, v.default) == Right(v), v.id)
    }
    val ids = fields.map(_.view.id).toSet
    check(ids.size == fields.size, s"repeated form field ids ${fields.map(_.view.id)}")
    check(values.values.keySet.subsetOf(ids), values.values.keySet -- ids)
    rebuilt.foreach(r => check(r == Right(description), r))
    // Every described field is edited by a form field or taken from the
    // context, so a description field without a form field fails here.
    val uncovered = description
      .map(d => generalised(d._1))
      .filterNot(k => covers.contains(k) || context.contains(k))
    check(uncovered.isEmpty, s"described fields no form field edits: $uncovered")
    check(
      covers.values.forall(ids.map(_.value).contains),
      covers.values.toSet -- ids.map(_.value)
    )

  final def preflight(): Unit =
    Vector(ready, unavailable).foreach(r => check(r.family == family, r.family))
    check(ready.ready, ready.diagnostics.map(_.code))
    check(!unavailable.ready, "preflight without input is ready")
    check(
      unavailable.diagnostics.exists(_.code.name == "missing-artifact"),
      unavailable.diagnostics.map(_.code)
    )
    (ready.diagnostics ++ unavailable.diagnostics).foreach(d => catalogued(d))

  private def catalogued(d: Diagnostic[Any]): Unit =
    check(DiagnosticCatalog.codes.contains(d.code), d.code)
    d.causes.foreach(catalogued)

  final def stepwise(): Unit =
    val expected = get(archived(get(run)))
    Vector(Finest, WorkQuanta.default).foreach { quanta =>
      val stepped = get(Stepwise.complete(get(cursor), quanta))
      check(
        get(archived(stepped)) == expected,
        s"Stepwise.complete at $quanta differs from run"
      )
    }
    // The cursor honours its quanta: the finest cut takes more, smaller steps.
    check(
      steps(Finest) > steps(WorkQuanta.default),
      s"${steps(Finest)} finest steps against ${steps(WorkQuanta.default)} default steps"
    )

  /** Steps the runner takes at `quanta`, from a pure trace. */
  private def steps(quanta: WorkQuanta): Int =
    @annotation.tailrec
    def loop(c: Cursor, n: Int): Int = get(stepping.advance(c, quanta)) match
      case WorkStep.More(_, _, next) => loop(next, n + 1)
      case WorkStep.Done(_, _)       => n + 1
    loop(get(cursor), 0)

  private def steps: Int = steps(Finest)

  final def completes(): IO[Unit] =
    Execution[IO].start(submission(Finest)).use(_.outcome).map {
      case RunOutcome.Completed(_, last, result) =>
        check(last.step == steps.toLong, s"completed after ${last.step} of $steps steps")
        check(get(archived(result)) == get(archived(get(run))), "Execution differs from run")
      case other => check(false, s"settled $other")
    }

  /** Park the runner at its between-step hook after `completed` steps, cancel
    * it there, and count the advances the cursor took.
    */
  final def cancels(): IO[Unit] =
    val total     = steps
    val completed = total / 2
    check(total >= 2, s"the fixture takes only $total step")
    var advances = 0
    val counting = new Stepwise[Cursor, Stage, Error, Result]:
      def stage(c: Cursor): Stage = stepping.stage(c)
      def advance(c: Cursor, q: WorkQuanta): Either[Error, WorkStep[Stage, Cursor, Result]] =
        advances += 1
        stepping.advance(c, q)
    val base = submission(Finest)
    val work = new Submission(base.id, base.quanta, base.begin, base.segment, base.total)(using
      counting
    )
    for
      started <- Ref[IO].of(0)
      reached <- Deferred[IO, Unit]
      between = started.updateAndGet(_ + 1).flatMap { n =>
        if n == completed + 1 then reached.complete(()) >> IO.never else IO.unit
      }
      outcome <- Execution[IO].start(work, between).use { run =>
        reached.get.race(run.outcome.void) >> run.cancel >> run.outcome
      }
    yield outcome match
      case RunOutcome.Cancelled(_, last) =>
        check(last.map(_.step) == Some(completed.toLong), s"cancelled at $last")
        check(advances == completed, s"the cursor advanced $advances times, not $completed")
      case other => check(false, s"cancelling after $completed of $total steps settled $other")

  final def archive(): Unit =
    val saved              = get(savedPlan)
    val (described, again) = get(reloadedPlan(saved))
    check(described == description, "the reloaded plan's description differs")
    check(again == saved, "the plan re-encodes differently")
    val document = get(archived(get(run)))
    check(get(rearchived(document)) == document, "the result re-encodes differently")

  final def table(): Either[Gap, Vector[Tabulated]] =
    tables(get(run)).map(get)

object FamilyWitness:
  /** An obligation of the definition of done. */
  enum Obligation derives CanEqual:
    case Inspect, Preflight, Stepwise, Execution, Archive, Table, Form

  /** A family's form on the fixture plan: its fields, the raw values the plan
    * projects to, and, for a family whose form builds a whole plan, the
    * description those values rebuild.
    */
  final case class FormEvidence(
      fields: Vector[FormField[?, ?]],
      values: FormValues,
      rebuilt: Option[Either[String, Vector[(String, Vector[Provenance.Param])]]],
      covers: Map[String, String],
      context: Set[String]
  )

  /** A description key with its index or a method parameter's name
    * generalised: `scale.3` is `scale.i`, `method.sigma` is `method.*`.
    */
  def generalised(key: String): String =
    val indexed = key.replaceAll("\\.[0-9]+$", ".i")
    Vector("method.", "detector.").find(indexed.startsWith).fold(indexed)(_ + "*")

  /** A projected table with what the result says it must hold: its row
    * count and, for a contrast table, the finite differences of the
    * `difference` column in row order.
    */
  final case class Tabulated(table: ResultTable, rows: Int, differences: Option[Vector[Double]])

  /** A study result's contrast rows: one per scale that failed, else one per
    * contrast row (similarity has one component), and its finite differences.
    */
  def contrastRows[K, U <: Unit2D](
      result: StudyResult[K, U, Similarity, SignedDifference]
  ): (Int, Vector[Double]) =
    result.scales.foldLeft((0, Vector.empty[Double])) { case ((rows, differences), scale) =>
      scale.contrast match
        case Left(_)         => (rows + 1, differences)
        case Right(contrast) =>
          (
            rows + contrast.rows.size,
            differences ++ contrast.rows.flatMap(_.difference.toOption.map(_.value))
          )
    }

  /** An obligation the family does not meet yet, and why. */
  final case class Gap(obligation: Obligation, reason: String)

  /** What the suite reads from a preflight report. */
  final case class Preflighted(
      family: RecipeFamily,
      ready: Boolean,
      diagnostics: Vector[Diagnostic[Any]]
  )
  object Preflighted:
    def of(report: PreflightReport[?], diagnostics: Vector[Diagnostic[Any]]): Preflighted =
      Preflighted(report.family, report.ready, diagnostics)

  private def quanta[E, A](e: Either[E, A]): A =
    e.fold(error => throw new AssertionError(s"$error"), identity)

  /** One pair, one comparison and one sample per step: the most steps. */
  val Finest: WorkQuanta = WorkQuanta(
    quanta(PairQuantum.of(1)),
    quanta(ComparisonQuantum.of(1)),
    quanta(SampleQuantum.of(1))
  )

/** The shipped families' witnesses. Import the givens to collect them. */
object FamilyWitnesses:
  import FamilyWitness.*

  /** One witness per case of `RecipeFamily`, in declaration order; a case
    * without a `given FamilyWitness` in scope is a compile error.
    */
  inline def all(using m: Mirror.SumOf[RecipeFamily]): Vector[FamilyWitness[?]] =
    summonAll[Tuple.Map[m.MirroredElemTypes, FamilyWitness]].toList.toVector
      .asInstanceOf[Vector[FamilyWitness[?]]]

  private def get[E, A](e: Either[E, A]): A =
    e.fold(error => throw new AssertionError(s"$error"), identity)

  private def definition(name: String): DefinitionId = get(DefinitionId.of(name, 1))

  // ------------------------------------------------------------------ fixtures

  private val frame = get(Frame.screen("conformance-display", 2, 2))
  private val grid  = get(Grid.over(frame, 2, 2))

  private def scanpath(key: StudyKey, points: (Double, Double)*): Scanpath[Px] =
    val clock = ClockId(s"${key.participant}/${key.stimulus}/${key.phase}")
    val fixes = points.zipWithIndex.map { case ((x, y), i) =>
      get(
        Event.Fixation.withoutDispersion(
          get(Interval.of(clock, Instant.micros(i * 1000L), Instant.micros(i * 1000L + 1000L))),
          Pt[Px](x, y),
          1
        )
      )
    }
    get(Scanpath.of(frame, clock, IArray.from(fixes)))

  private val studyInput: StudyInput[StudyKey, Px] =
    val rows = Vector(
      StudyKey("p1", "a", "recall") -> Vector((0.5, 0.5), (1.5, 0.5), (0.5, 1.5)),
      StudyKey("p1", "b", "recall") -> Vector((1.5, 1.5), (0.5, 1.5)),
      StudyKey("p1", "a", "encode") -> Vector((0.5, 0.5), (1.5, 0.5)),
      StudyKey("p1", "b", "encode") -> Vector((1.5, 1.5), (1.5, 0.5), (0.5, 0.5)),
      StudyKey("p2", "a", "recall") -> Vector((1.5, 0.5), (0.5, 0.5)),
      StudyKey("p2", "a", "encode") -> Vector((1.5, 0.5), (1.5, 1.5))
    )
    StudyInput(Trials(rows.map((key, points) => Trial(key, (), scanpath(key, points*)))))

  private val studyPlan = get(
    StudyPlan.cosine(
      studyInput.reference,
      grid,
      "recall",
      "encode",
      Weight.Duration,
      Vector(
        StudyEstimate.Binned(),
        StudyEstimate.Gaussian(get(Sigma.px(0.5)), EdgePolicy.Truncate)
      ),
      FailurePolicy.RequireAll
    )
  )

  private val temporalInput: TemporalStudyInput[StudyKey, Px] =
    get(
      TemporalStudyInput.of(
        studyInput,
        studyInput.trials.rows.map { row =>
          val clock = row.value.clock
          row.key -> TrialEpoch(
            Instant.micros(0),
            get(
              ObservedCoverage.of(
                clock,
                Vector(get(Interval.of(clock, Instant.micros(0), Instant.micros(3000))))
              )
            )
          )
        }
      )
    )

  private val temporalSchema = definition("eyes4s.temporal-study")
  private val temporalPlan   = get(
    TemporalStudyPlan.of(
      studyPlan,
      temporalInput.reference,
      Vector(
        get(StudyWindow.of("early", get(Window.of(Span.micros(0), Span.micros(1500))))),
        get(StudyWindow.of("late", get(Window.of(Span.micros(1500), Span.micros(3000)))))
      ),
      Vector(get(RepetitionContrast.withinParticipant("recall", "recall", "encode"))),
      FixationBoundary.ClipDuration
    )
  )

  private val display       = get(Frame.screen("conformance-screen", 1000, 1000))
  private val trackerClock  = ClockId("conformance-tracker")
  private val analysisClock = ClockId("conformance-analysis")

  /** 40 samples at 100 Hz: two fixations around a saccade and a two-sample gap. */
  private def gaze(i: Int): Gaze[Px] =
    if i == 25 || i == 26 then Gaze.Lost()
    else if i < 15 then Gaze.Tracked(Pt[Px](400.0, 500.0), None)
    else if i == 15 then Gaze.Tracked(Pt[Px](480.0, 500.0), None)
    else if i == 16 then Gaze.Tracked(Pt[Px](560.0, 500.0), None)
    else Gaze.Tracked(Pt[Px](600.0, 500.0), None)

  private val samples = get(
    Recording.of(
      display,
      trackerClock,
      Rate.Fixed(get(Hz(100.0))),
      Eye.Left,
      None,
      IArray.from((0 until 40).map(i => Sample(Instant.millis(i.toLong * 10L), gaze(i))))
    )
  )

  private val recordingCodec = RecordingCodecs.ivt(
    definition("eyes4s.recording-plan"),
    definition("eyes4s.recording.ivt"),
    definition("eyes4s.ivt-parameters")
  )

  private val recordingPlan = get(
    RecordingPlan.of(
      ArtifactRef.of(samples.contentHash),
      RecordingRef("conformance-recording"),
      display,
      trackerClock,
      analysisClock,
      FrameId("conformance-angular"),
      Some(get(Viewing.millimetres(600.0, 500.0, 500.0))),
      SyncFitMode.OffsetOnly,
      Vector(
        get(SyncMark.of("start", Instant.millis(0), Instant.millis(0))),
        get(SyncMark.of("end", Instant.millis(390), Instant.millis(390)))
      ),
      None,
      get(InterpolationGap.of(Span.micros(30000))),
      Vector(
        get(
          RecordingArea
            .of("centre", "Central area", get(Bounds.of[Px](300.0, 300.0, 700.0, 700.0)))
        )
      ),
      recordingCodec.method,
      IvtParameters(
        get(IvtThreshold.of(get(Velocity.perSecond[Deg](30)))),
        get(MinimumEventDuration.of(Span.micros(20000)))
      )
    )
  )

  // ------------------------------------------------------------------ witnesses

  /** Study description keys and the form field that edits each. */
  private val studyCovers: Map[String, String] = Map(
    "phases"           -> "phases",
    "weight"           -> "weight",
    "failurePolicy"    -> "failurePolicy",
    "grid"             -> "grid",
    "window"           -> "window",
    "offWindow"        -> "offWindow",
    "angularScale"     -> "angularScale",
    "scale.i"          -> "scales",
    "estimate.i"       -> "scales",
    "pairing"          -> "pairing",
    "initialFixations" -> "initialFixations"
  )

  /** Study description keys the form takes from its context: the input,
    * layout, method (and its parameters) and the admission frame.
    */
  private val studyContext: Set[String] =
    Set("input", "layout", "method", "method.*", "frame", "admission")

  given study: FamilyWitness[RecipeFamily.FixationStudy.type] =
    new FamilyWitness[RecipeFamily.FixationStudy.type](RecipeFamily.FixationStudy):
      type Id      = eyes4s.fs2.StudyRunId
      type Cursor  = StudyCursor[StudyKey, Px, Similarity, SignedDifference]
      type Stage   = StudyStage
      type Segment = StudySegment
      type Error   = PlanError
      type Result  = StudyResult[StudyKey, Px, Similarity, SignedDifference]

      private val plans    = StudyCodecs.cosine[Px]
      private val results  = StudyResultCodecs.cosine[Px]
      private val prepared = get(studyPlan.prepare(studyInput))

      def description = studyPlan.description
      def inspection  = studyPlan.inspect
      def ready       =
        val report = studyPlan.preflight(Some(studyInput))
        Preflighted.of(report, report.diagnostics)
      def unavailable =
        val report = studyPlan.preflight(None)
        Preflighted.of(report, report.diagnostics)
      def run                            = prepared.run
      def cursor                         = prepared.work()
      def submission(quanta: WorkQuanta) = StudyExecution.submission(prepared, quanta = quanta)
      given stepping: Stepwise[Cursor, Stage, Error, Result] = Stepwise.study
      def archived(result: Result)                           = results.codec.encode(result)
      def rearchived(document: Json)                         =
        results.codec.decode(document).flatMap(results.codec.encode)
      def savedPlan                    = plans.codec.encode(studyPlan)
      def reloadedPlan(document: Json) =
        plans.codec.decode(document).flatMap(p => plans.codec.encode(p).map(p.description -> _))
      def tables(result: Result) =
        Right(
          ResultExports
            .study(studyPlan, result, plans, ScoreColumns.similarity)
            .map { table =>
              val (rows, differences) = contrastRows(result)
              Vector(Tabulated(table, rows, Some(differences)))
            }
        )
      def form =
        val f      = new StudyForm(StudyFormContext.of(studyPlan))
        val values = f.values(studyPlan)
        FormEvidence(
          f.fields.map(_._2),
          values,
          Some(
            f.parse(values)
              .left
              .map(_.toVector.map(_.message).mkString("; "))
              .flatMap(
                _.plan(
                  studyPlan.input,
                  studyPlan.layout,
                  studyPlan.method,
                  studyPlan.parameters
                ).left
                  .map(_.message)
              )
              .map(_.description)
          ),
          studyCovers,
          studyContext
        )

  given recording: FamilyWitness[RecipeFamily.EventRecording.type] =
    new FamilyWitness[RecipeFamily.EventRecording.type](RecipeFamily.EventRecording):
      type Id      = eyes4s.fs2.RecordingRunId
      type Cursor  = RecordingCursor[IvtParameters]
      type Stage   = RecordingStage
      type Segment = RecordingSegment
      type Error   = RecordingPlanError
      type Result  = RecordingAnalysis[IvtParameters]

      private val results = recordingCodec.results

      def description = recordingPlan.description
      def inspection  = recordingPlan.inspect
      def ready       =
        val report = recordingPlan.preflight(Some(samples))
        Preflighted.of(report, report.diagnostics)
      def unavailable =
        val report = recordingPlan.preflight(None)
        Preflighted.of(report, report.diagnostics)
      def run                            = recordingPlan.run(samples)
      def cursor                         = recordingPlan.work(samples)
      def submission(quanta: WorkQuanta) =
        RecordingExecution.submission(recordingPlan, samples, quanta)
      given stepping: Stepwise[Cursor, Stage, Error, Result] = Stepwise.recording
      def archived(result: Result)                           = results.codec.encode(result)
      def rearchived(document: Json)                         =
        results.codec.decode(document).flatMap(results.codec.encode)
      def savedPlan                    = recordingCodec.codec.encode(recordingPlan)
      def reloadedPlan(document: Json) =
        recordingCodec.codec
          .decode(document)
          .flatMap(p => recordingCodec.codec.encode(p).map(p.description -> _))
      def tables(result: Result) =
        Left(
          Gap(
            Obligation.Table,
            "a recording analysis has no ResultTable projection: ResultExports tabulates " +
              "study and temporal contrasts only, and eyes4s-results has no event table family"
          )
        )
      def form =
        val f        = new RecordingForm
        val detector = recordingPlan.method.descriptor.toVector.flatMap(_.parameters.fields)
        FormEvidence(
          f.fields ++ detector.flatMap(_.descriptor.form),
          FormValues.from(
            f.values(recordingPlan).values ++ recordingPlan.method.descriptor
              .fold(FormValues.empty)(_.parameters.formValues(recordingPlan.parameters))
              .values
          ),
          Some(
            f.parse(f.values(recordingPlan))
              .left
              .map(_.toVector.map(_.message).mkString("; "))
              .flatMap(_.plan(recordingPlan).left.map(_.message))
              .map(_.description)
          ),
          Map(
            "viewing"                -> "viewing",
            "syncModel"              -> "syncModel",
            "residualLimitMicros"    -> "residualLimitMicros",
            "interpolationGapMicros" -> "interpolationGapMicros",
            "sync.i"                 -> "marks",
            "area.i"                 -> "areas"
          ) ++ detector.headOption.map(f => "detector.*" -> f.view.id.value),
          Set("input", "source", "frame", "clocks", "angularFrame", "detector")
        )

  given temporal: FamilyWitness[RecipeFamily.TemporalStudy.type] =
    new FamilyWitness[RecipeFamily.TemporalStudy.type](RecipeFamily.TemporalStudy):
      type Id      = eyes4s.fs2.TemporalRunId
      type Cursor  = TemporalCursor[StudyKey, Px, Unit, Similarity, SignedDifference]
      type Stage   = TemporalStage
      type Segment = TemporalSegment
      type Error   = TemporalStudyError
      type Result  = TemporalStudyResult[StudyKey, Px, Unit, Similarity, SignedDifference]

      private val plans    = new TemporalStudyCodec(temporalSchema, StudyCodecs.cosine[Px])
      private val results  = TemporalResultCodecs.cosine[Px](temporalSchema)
      private val prepared = get(temporalPlan.prepare(temporalInput))

      def description = temporalPlan.description
      def inspection  = temporalPlan.inspect
      def ready       =
        val report = temporalPlan.preflight(Some(temporalInput))
        Preflighted.of(report, report.diagnostics)
      def unavailable =
        val report = temporalPlan.preflight(None)
        Preflighted.of(report, report.diagnostics)
      def run                            = prepared.run
      def cursor                         = prepared.work()
      def submission(quanta: WorkQuanta) =
        TemporalExecution.submission(prepared, quanta = quanta)
      given stepping: Stepwise[Cursor, Stage, Error, Result] = Stepwise.temporal
      def archived(result: Result)                           = results.codec.encode(result)
      def rearchived(document: Json)                         =
        results.codec.decode(document).flatMap(results.codec.encode)
      def savedPlan                    = plans.codec.encode(temporalPlan)
      def reloadedPlan(document: Json) =
        plans.codec.decode(document).flatMap(p => plans.codec.encode(p).map(p.description -> _))
      def tables(result: Result) =
        Right(
          ResultExports.temporal(temporalPlan, result, plans, ScoreColumns.similarity).map {
            tables =>
              val contrasts = result.cells.map(cell => contrastRows(cell.result))
              val coverage  = result.cells.map(_.occupancy.size).sum
              tables.map {
                case t if t.family == ResultFamily.TemporalContrasts =>
                  Tabulated(t, contrasts.map(_._1).sum, Some(contrasts.flatMap(_._2)))
                case t => Tabulated(t, coverage, None)
              }
          }
        )
      def form =
        val base = new StudyForm(StudyFormContext.of(temporalPlan.base))
        val own  = new TemporalForm
        FormEvidence(
          base.fields.map(_._2) ++ own.fields,
          FormValues.from(
            base.values(temporalPlan.base).values ++ own.values(temporalPlan).values
          ),
          Some(
            own
              .parse(own.values(temporalPlan))
              .left
              .map(_.toVector.map(_.message).mkString("; "))
              .flatMap(_.plan(temporalPlan.base, temporalPlan.input).left.map(_.message))
              .map(_.description)
          ),
          studyCovers ++ Map(
            "temporal.boundary" -> "temporal.boundary",
            "window.i"          -> "windows",
            "repetition.i"      -> "repetitions"
          ),
          studyContext ++ Set("temporal.input", "temporal.scope")
        )
