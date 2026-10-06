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

package eyes4s.studio.core.command

import eyes4s.plan.AdmissionDecision as CoreAdmissionDecision
import eyes4s.codec.ByteDigest
import eyes4s.studio.core.assets.{AssetFile, AssetRef}
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.*
import eyes4s.studio.core.document.DocumentGen.*
import eyes4s.studio.core.preset.InitialRecipe
import org.scalacheck.Gen

import scala.compiletime.constValueTuple
import scala.deriving.Mirror

/** Commands aimed at a document's actual contents, so most of them apply,
  * plus some that are refused; and sessions of journal entries over them.
  */
object CommandGen:

  /** Every case name of an enum, from its mirror. */
  inline def caseNames[T](using m: Mirror.SumOf[T]): Vector[String] =
    constValueTuple[m.MirroredElemLabels].toList.map(_.toString).toVector

  val allCommands: Vector[String] = caseNames[Command]

  /** Commands that never enter an undo stack. */
  val irreversible: Set[String] =
    Set(
      "Admit",
      "SaveAndRun",
      "RecordRunOutcome",
      "CancelRun",
      "BindPlan",
      "BindCompletedArtifacts"
    )

  private def pick[A](values: Vector[A]): Option[Gen[A]] =
    Option.when(values.nonEmpty)(Gen.oneOf(values))

  private val finished: Gen[RunLifecycle] = lifecycle.suchThat(_ != RunLifecycle.Running)

  private def scalesOf(d: StudioDocument, run: RunId): Option[ScaleSet] =
    d.run(run).flatMap(r => d.analysis(r.analysis)).map(_.recipe.scales)

  private def verifying(spec: DatasetRevisionSpec) = spec.decision match
    case AdmissionDecision.Verifying(content) => Some(content)
    case _                                    => None

  def dataset(d: StudioDocument): Vector[Gen[Command]] =
    val ids       = d.datasets.map(_.id)
    val pending   = d.datasets.filterNot(_.decision.isAdmitted)
    val editable  = pending.filter(_.decision == AdmissionDecision.Pending)
    val next      = d.datasets.lastOption.fold(1)(_.id.number + 1)
    val importing = for
      parent <- if ids.isEmpty then Gen.const(None) else Gen.option(Gen.oneOf(ids))
      s      <- sources
      m      <- mapping
      u      <- units
      g      <- geometry
      a      <- attributesFor(m)
      c      <- Gen.option(admission)
      i      <- inventoryFor(s)
    yield Command.ImportSources(parent, s, m, u, g, a, c, i)
    val edits = pick(editable).toVector.flatMap { specs =>
      Vector(
        specs.flatMap(s => mapping.map(Command.SetMapping(s.id, _))),
        specs.flatMap(s => units.map(Command.SetUnits(s.id, _))),
        specs.flatMap(s => geometry.map(Command.SetGeometry(s.id, _))),
        for
          s <- specs
          m <- mapping
          u <- units
          g <- geometry
          a <- attributesFor(m)
        yield Command.ReviseDataset(
          s.id,
          m,
          u,
          g,
          a,
          s.inventory.orElse(storedInventory(s.sources))
        ),
        specs.flatMap(s =>
          Gen.oneOf(OffScreenChoice.values.toSeq).map(Command.SetOffScreenPolicy(s.id, _))
        ),
        for
          s    <- specs
          i    <- Gen.choose(-1, s.admission.corrections.size + 1)
          rule <- Gen.zip(target, correction).map(CorrectionRule.apply)
        yield Command.AddCorrection(s.id, i, rule),
        for
          s <- specs
          i <- Gen.choose(-1, s.admission.corrections.size)
        yield Command.RemoveCorrection(s.id, i)
      )
    }
    val lifecycle = pick(pending).toVector.flatMap { specs =>
      Vector(
        specs.map(s => Command.VerifyDataset(s.id)),
        specs.map(s => Command.WithdrawVerification(s.id)),
        specs.flatMap(s =>
          canonical[DatasetRevisionSpec].map(Command.ResumeVerification(s.id, _))
        ),
        for
          // Mostly a revision sent for verification, which an admission can
          // apply to: the coverage law needs some admission to apply.
          s <- pick(pending.filter(verifying(_).isDefined))
            .fold(specs)(v => Gen.frequency(3 -> v, 1 -> specs))
          v <- verifying(s).fold(canonical[DatasetRevisionSpec])(c =>
            Gen.frequency(4 -> Gen.const(c), 1 -> canonical[DatasetRevisionSpec])
          )
          l <- binding[AdmissionLedgerArtifact]
          t <- binding[TrialInventoryArtifact]
          p <- Gen.option(Gen.oneOf(CoreAdmissionDecision.values.toVector))
        yield Command.Admit(s.id, v, p, l, t),
        specs.map(s => Command.DiscardDataset(s.id)),
        specs.map(s => Command.RestoreDataset(s.copy(id = DatasetRevision(next)))),
        specs.map(s =>
          Command.RestoreRepairedDataset(
            s.copy(id = DatasetRevision(next)),
            Vector(
              AssetRelink(
                DatasetRevision(next),
                AssetFile.of("forest-044.png").toOption.get,
                AssetRef(
                  AssetFile.of("repaired-ab.png").toOption.get,
                  ByteDigest.parse("ab" * 32).toOption.get
                )
              )
            )
          )
        )
      )
    }
    importing +: (edits ++ lifecycle)

  /** Backend facts and requests: never in an undo stack. */
  def backend(d: StudioDocument): Vector[Gen[Command]] =
    val outcomes = pick(d.running).toVector.flatMap { runs =>
      Vector(
        for
          r <- runs
          s <- finished
          a <- binding[ResultArchiveArtifact]
        yield Command.RecordRunOutcome(r.id, s, a),
        runs.map(r => Command.CancelRun(r.id))
      )
    }
    val plans = pick(d.analyses).toVector.map { specs =>
      for
        a <- specs
        p <- canonical[StudyPlanArtifact]
        i <- a.recipe.input.fold(semantic)(i => Gen.frequency(3 -> Gen.const(i), 1 -> semantic))
      yield Command.BindPlan(a.id, p, i)
    }
    val artifacts = pick(d.runs).toVector.map { runs =>
      for
        run <- runs
        spec = d.analysis(run.analysis).get
        plan <- spec.plan match
          case CoreBinding.Bound(value) => Gen.const(value)
          case _                        => canonical[StudyPlanArtifact]
        input  <- canonical[eyes4s.studio.core.execution.StudyInputArtifact]
        result <- run.archive match
          case CoreBinding.Bound(value) => Gen.const(value)
          case _                        => canonical[ResultArchiveArtifact]
        source <- spec.recipe.input
          .orElse(d.dataset(spec.dataset).flatMap(_.sources.fixations).flatMap(_.semantic))
          .fold(semantic)(Gen.const(_))
        facts = eyes4s.studio.core.artifacts.NativeBindingFacts
          .of(
            run.id,
            eyes4s.studio.core.execution.RunStamp(
              spec.id,
              spec.dataset,
              CoreBinding.Bound(plan),
              CoreBinding.Bound(input)
            ),
            source,
            result,
            spec.recipe
          )
          .toOption
          .get
      yield Command.BindCompletedArtifacts(facts)
    }
    outcomes ++ plans ++ artifacts

  def analysis(d: StudioDocument): Vector[Gen[Command]] =
    val admitted = d.datasets.filter(_.decision.isAdmitted).map(_.id)
    val initial  =
      if d.analyses.nonEmpty || d.draft.nonEmpty then Vector.empty
      else
        pick(d.datasets.filter(_.decision.isAdmitted)).toVector.map { datasets =>
          for
            data   <- datasets
            preset <- Gen.oneOf(
              Preset.EncodingRetrieval,
              Preset.PerceptionImagery,
              Preset.Recognition
            )
            seed = right(InitialRecipe.of(data, preset))
          yield Command.StartAnalysis(data.id, seed._1, seed._2)
        }
    val current = d.draftRecipe.orElse(d.latestAnalysis.map(_.recipe))
    val changes = current.toVector.map { now =>
      recipe
        .map(RecipeChange.between(now, _))
        .suchThat(_.nonEmpty)
        .flatMap(cs => Gen.oneOf(cs))
        .map(Command.ChangeRecipe(_))
    }
    // A composite edit: every field a generated recipe changes, at once.
    val composites = current.toVector.map { now =>
      recipe
        .map(RecipeChange.between(now, _))
        .suchThat(_.nonEmpty)
        .map(Command.ChangeRecipes(_))
    }
    val reverts = d.draft.flatMap(dr => pick(dr.changes)).toVector.map { g =>
      g.map(c => Command.ChangeRecipe(c.inverse))
    }
    val starts = pick(d.analyses).toVector.map { bases =>
      for
        base <- bases
        r    <- recipe
        ds   <- Gen.option(Gen.oneOf(d.datasets.map(_.id)))
      yield Command.StartDraft(base.id, ds, RecipeChange.between(base.recipe, r))
    }
    val restores = d.latestAnalysis.toVector.flatMap { latest =>
      pick(d.analyses).toVector.map { bases =>
        bases
          .flatMap(b => draft(b, latest.id.number + 1, admitted))
          .suchThat(_.nonEmpty)
          .map(dr => Command.RestoreDraft(dr.get))
      }
    }
    val rebases = pick(d.datasets.map(_.id)).toVector.map(_.map(Command.RebaseDraft(_)))
    initial ++ changes ++ composites ++ reverts ++ starts ++ restores ++ rebases ++ Vector(
      Gen.const(Command.DiscardDraft),
      Gen.option(studio).map(Command.SaveAndRun(_))
    )

  def reportingAndFigures(d: StudioDocument): Vector[Gen[Command]] =
    val reportingIds = d.reporting.map(_.id)
    val put          = Gen
      .oneOf(reportingIds.map(_.value) :+ s"rep-${d.reporting.size + 7}")
      .flatMap(reporting)
      .map(Command.PutReporting(_))
    val remove     = pick(reportingIds).toVector.map(_.map(Command.RemoveReporting(_)))
    val nextFigure = right(FigureId.of(d.figures.lastOption.fold(1)(_.id.number + 1)))
    val binds      = for
      runs <- pick(d.runs.map(_.id)).toVector
      reps <- pick(reportingIds).toVector
    yield
      for
        run    <- runs
        rep    <- reps
        ps     <- panels(scalesOf(d, run).get)
        create <- Gen.oneOf(
          Command.CreateFigure(run, rep, ps),
          Command.RestoreFigure(right(FigureSpec.of(nextFigure, run, rep, ps)))
        )
      yield create
    val figureEdits = pick(d.figures).toVector.flatMap { figures =>
      Vector(
        figures.map(f => Command.DeleteFigure(f.id)),
        for
          f   <- figures
          run <- Gen.oneOf(d.runs.map(_.id))
          rep <- Gen.oneOf(reportingIds)
        yield Command.BindFigure(f.id, run, rep),
        for
          f <- figures
          p <- Gen.oneOf(f.panels)
          s <- panelScale(scalesOf(d, f.run).get)
        yield Command.SetPanelScale(f.id, p.letter, s),
        for
          f <- figures
          p <- Gen.oneOf(f.panels)
          s <- selection
        yield Command.SetPanelSelection(f.id, p.letter, s),
        for
          f      <- figures
          letter <- Gen.oneOf('A' to 'H').map(l => right(PanelLetter.of(l.toString)))
          i      <- Gen.choose(-1, f.panels.size + 1)
          t      <- text
          s      <- panelScale(scalesOf(d, f.run).get)
          sel    <- selection
        yield Command.AddPanel(f.id, i, PanelSpec(letter, t, s, sel)),
        for
          f <- figures
          p <- Gen.oneOf(f.panels)
        yield Command.RemovePanel(f.id, p.letter),
        for
          f <- figures
          p <- Gen.oneOf(f.panels)
          t <- text
        yield Command.RetitlePanel(f.id, p.letter, t)
      )
    }
    (put +: remove) ++ binds ++ figureEdits

  def view(d: StudioDocument): Vector[Gen[Command]] =
    Vector(
      Gen.oneOf(Perspective.values.toSeq).map(Command.SetPerspective(_)),
      Gen.oneOf(Theme.values.toSeq).map(Command.SetTheme(_)),
      Gen.oneOf(StageAppearance.values.toSeq).map(Command.SetStage(_)),
      Gen.oneOf(0.0, 0.25, 0.6, 1.0).map(o => Command.SetMapOpacity(right(MapOpacity.of(o)))),
      Gen.oneOf(true, false).map(Command.SetUnderlay(_)),
      Gen.option(Gen.oneOf(d.runs.map(_.id) :+ RunId(99))).map(Command.ShowRun(_)),
      for
        p <- Gen.oneOf(Perspective.values.toSeq)
        l <- Gen.option(text.map(LayoutBlob(_)))
      yield Command.SaveLayout(p, l)
    )

  /** Repairs (S5.7): a file of a dataset revision (or an unknown one)
    * relinked to a stored asset, or unlinked.
    */
  def assets(d: StudioDocument): Vector[Gen[Command]] =
    val files =
      Vector("forest-044.png", "kitchen-081.png").map(f => AssetFile.of(f).toOption.get)
    val stored = Gen
      .oneOf("ab", "cd")
      .map(h =>
        AssetRef(
          AssetFile.of(s"repaired-$h.png").toOption.get,
          ByteDigest.parse(h * 32).toOption.get
        )
      )
    Vector(
      for
        // Mostly a held revision and a new link: an unknown revision, or an
        // unlink of nothing, is refused, and refusals are rarer than edits.
        id <-
          if d.datasets.isEmpty then Gen.const(DatasetRevision(99))
          else
            Gen.frequency(
              9 -> Gen.oneOf(d.datasets.map(_.id)),
              1 -> Gen.const(DatasetRevision(99))
            )
        file  <- Gen.oneOf(files)
        asset <- Gen.frequency(4 -> stored.map(Some(_)), 1 -> Gen.const(None))
      yield Command.RelinkAsset(id, file, asset)
    )

  private def among(groups: Vector[Gen[Command]]*): Gen[Command] =
    Gen.oneOf(groups.filter(_.nonEmpty)).flatMap(gs => Gen.oneOf(gs)).flatMap(identity)

  /** A command for `d`, from any group. */
  def command(d: StudioDocument): Gen[Command] =
    among(dataset(d), analysis(d), backend(d), reportingAndFigures(d), view(d), assets(d))

  /** A science edit: not view-only and not a backend fact. */
  def edit(d: StudioDocument): Gen[Command] =
    among(dataset(d), analysis(d), reportingAndFigures(d), assets(d))

  /** What may happen between an edit and its undo without undoing it: a
    * backend fact or request, or (when the edit is science) a view edit.
    */
  def interleaved(d: StudioDocument, science: Boolean): Option[Gen[Command]] =
    val groups = if science then Vector(backend(d), view(d)) else Vector(backend(d))
    Option.when(groups.exists(_.nonEmpty))(among(groups*))

  /** Up to `n` interleaved commands, each drawn for the document it meets. */
  def interleave(h: History, n: Int, science: Boolean): Gen[Vector[Command]] =
    interleaved(h.document, science).filter(_ => n > 0).fold(Gen.const(Vector.empty)) { g =>
      g.flatMap { c =>
        interleave(h.apply(c).fold(_ => h, _.history), n - 1, science).map(c +: _)
      }
    }

  /** A history reached by a walk, a command for it, and what is interleaved
    * after the command.
    */
  val interleaving: Gen[(History, Command, Vector[Command])] =
    for
      (d, traces) <- session(6)
      h = traces.lastOption.fold(History.start(d))(t => t.result.fold(_ => t.before, _.history))
      c  <- command(h.document)
      xs <- h
        .apply(c)
        .fold(
          _ => Gen.const(Vector.empty),
          s => interleave(s.history, 4, c.kind != ChangeKind.ViewOnly)
        )
    yield (h, c, xs)

  /** A session entry: mostly commands, sometimes undo or redo. */
  def entry(h: History): Gen[JournalEntry] =
    Gen.frequency(
      12 -> command(h.document).map(JournalEntry.Apply(_)),
      2  -> Gen.const(JournalEntry.Undo),
      1  -> Gen.const(JournalEntry.Redo),
      1  -> Gen.const(JournalEntry.UndoView),
      1  -> Gen.const(JournalEntry.RedoView)
    )

  /** A step of a walk: the history before, the entry, and its result. */
  final case class Trace(
      before: History,
      entry: JournalEntry,
      result: Either[CommandError, Step]
  )

  /** `n` entries performed in turn from `h`; a refused entry leaves `h`. */
  def walk(h: History, n: Int, next: History => Gen[JournalEntry] = entry): Gen[Vector[Trace]] =
    if n == 0 then Gen.const(Vector.empty)
    else
      next(h).flatMap { e =>
        val result = h.perform(e)
        walk(result.fold(_ => h, _.history), n - 1, next).map(Trace(h, e, result) +: _)
      }

  /** Legal first-study documents: admitted data, no synthetic saved base,
    * with or without a checked initial working recipe. These enter the same
    * session walks, inverse laws and interleaving laws as existing studies.
    */
  private lazy val firstStudy: Gen[StudioDocument] =
    for
      n         <- Gen.choose(1, 3)
      generated <- Gen.sequence[Vector[DatasetRevisionSpec], DatasetRevisionSpec](
        (1 to n).map(i => DocumentGen.dataset(i, (1 until i).toVector))
      )
      datasets = generated.updated(
        0,
        generated.head.copy(decision =
          AdmissionDecision.Admitted(
            Some(CoreAdmissionDecision.ReviewExclusions),
            CoreBinding.unbound,
            CoreBinding.unbound
          )
        )
      )
      initial <- Gen.frequency(
        2 -> Gen.const(None),
        1 -> (for
          data   <- Gen.oneOf(datasets.filter(_.decision.isAdmitted))
          preset <- Gen
            .oneOf(Preset.EncodingRetrieval, Preset.PerceptionImagery, Preset.Recognition)
          seed = right(InitialRecipe.of(data, preset))
        yield Some(right(Draft.initial(AnalysisRevision(1), data.id, seed._1, seed._2))))
      )
      view <- presentation(Vector.empty)
    yield right(
      StudioDocument.of(
        datasets,
        Vector.empty,
        initial,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        view,
        Vector.empty
      )
    )

  private def fromDocument(
      start: Gen[StudioDocument],
      n: Int,
      next: History => Gen[JournalEntry]
  ): Gen[(StudioDocument, Vector[Trace])] =
    for
      d <- start
      t <- walk(History.start(d), n, next)
    yield (d, t)

  /** The original seeded population, retained for deterministic coverage. */
  def legacySession(
      n: Int,
      next: History => Gen[JournalEntry] = entry
  ): Gen[(StudioDocument, Vector[Trace])] = fromDocument(document, n, next)

  /** Genuine first-study histories, including initial working origins. */
  def firstStudySession(
      n: Int,
      next: History => Gen[JournalEntry] = entry
  ): Gen[(StudioDocument, Vector[Trace])] = fromDocument(firstStudy, n, next)

  /** Both populations exercise the same generated command laws. */
  def session(
      n: Int,
      next: History => Gen[JournalEntry] = entry
  ): Gen[(StudioDocument, Vector[Trace])] =
    Gen.frequency(3 -> legacySession(n, next), 1 -> firstStudySession(n, next))
