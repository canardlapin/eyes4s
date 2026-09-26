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

import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.*
import eyes4s.studio.core.document.DocumentGen.*
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

  private def pick[A](values: Vector[A]): Option[Gen[A]] =
    Option.when(values.nonEmpty)(Gen.oneOf(values))

  private val finished: Gen[RunLifecycle] = lifecycle.suchThat(_ != RunLifecycle.Running)

  private def scalesOf(d: StudioDocument, run: RunId): Option[ScaleSet] =
    d.run(run).flatMap(r => d.analysis(r.analysis)).map(_.recipe.scales)

  def dataset(d: StudioDocument): Vector[Gen[Command]] =
    val ids       = d.datasets.map(_.id)
    val pending   = d.datasets.filterNot(_.decision.isAdmitted)
    val next      = d.datasets.lastOption.fold(1)(_.id.number + 1)
    val importing = for
      parent <- if ids.isEmpty then Gen.const(None) else Gen.option(Gen.oneOf(ids))
      s      <- sources
      m      <- mapping
      u      <- units
      g      <- geometry
    yield Command.ImportSources(parent, s, m, u, g)
    importing +: pick(pending).toVector.flatMap { specs =>
      Vector(
        specs.flatMap(s => mapping.map(Command.SetMapping(s.id, _))),
        specs.flatMap(s => units.map(Command.SetUnits(s.id, _))),
        specs.flatMap(s => geometry.map(Command.SetGeometry(s.id, _))),
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
        yield Command.RemoveCorrection(s.id, i),
        specs.map(s => Command.VerifyDataset(s.id)),
        for
          s <- specs
          l <- binding[AdmissionLedgerArtifact]
          t <- binding[TrialInventoryArtifact]
        yield Command.Admit(s.id, l, t),
        specs.map(s => Command.DiscardDataset(s.id)),
        specs.map(s => Command.RestoreDataset(s.copy(id = DatasetRevision(next))))
      )
    }

  def analysis(d: StudioDocument): Vector[Gen[Command]] =
    val admitted = d.datasets.filter(_.decision.isAdmitted).map(_.id)
    val current  = d.draftRecipe.orElse(d.latestAnalysis.map(_.recipe))
    val changes  = current.toVector.map { now =>
      recipe
        .map(RecipeChange.between(now, _))
        .suchThat(_.nonEmpty)
        .flatMap(cs => Gen.oneOf(cs))
        .map(Command.ChangeRecipe(_))
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
    val rebases  = pick(d.datasets.map(_.id)).toVector.map(_.map(Command.RebaseDraft(_)))
    val outcomes = pick(d.running).toVector.map { runs =>
      for
        r <- runs
        s <- finished
        a <- binding[ResultArchiveArtifact]
      yield Command.RecordRunOutcome(r.id, s, a)
    }
    changes ++ reverts ++ starts ++ restores ++ rebases ++ outcomes ++ Vector(
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
        yield Command.SetPanelSelection(f.id, p.letter, s)
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

  /** A command for `d`, from any of the four kinds. */
  def command(d: StudioDocument): Gen[Command] =
    Gen
      .oneOf(dataset(d), analysis(d), reportingAndFigures(d), view(d))
      .flatMap(gs => Gen.oneOf(gs))
      .flatMap(identity)

  /** A command that is not view-only and not a backend fact. */
  def edit(d: StudioDocument): Gen[Command] =
    Gen
      .oneOf(dataset(d), analysis(d), reportingAndFigures(d))
      .flatMap(gs => Gen.oneOf(gs))
      .flatMap(identity)
      .suchThat(!_.isInstanceOf[Command.RecordRunOutcome])

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

  /** A generated document and a walk over it. */
  def session(
      n: Int,
      next: History => Gen[JournalEntry] = entry
  ): Gen[(StudioDocument, Vector[Trace])] =
    for
      d <- document
      t <- walk(History.start(d), n, next)
    yield (d, t)
