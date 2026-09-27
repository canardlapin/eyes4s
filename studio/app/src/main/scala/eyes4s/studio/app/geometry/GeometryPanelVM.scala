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

package eyes4s.studio.app.geometry

import eyes4s.studio.app.AppModel
import eyes4s.studio.app.text.{Format, GeometryText, GeometryTextId}
import eyes4s.studio.core.backend.{AdmissionSummary, TrialKey}
import eyes4s.studio.core.command.ChangeKind
import eyes4s.studio.core.document.*
import eyes4s.studio.core.importing.GeometryField
import eyes4s.studio.core.selection.{RecordNumber, StudioRef, TallyRegion}

/** One fact of the declared geometry ("Screen", "1920 × 1080 px"). */
final case class FactVM(label: String, value: String, mono: Boolean, note: Option[String])
    derives CanEqual

/** One typed field of the declared geometry. */
final case class FieldVM(field: GeometryField, label: String, value: String) derives CanEqual

/** One row of the worked example ("→ image frame", "(700, 300) px · …"). */
final case class ExampleRowVM(label: String, value: String) derives CanEqual

/** A representative trial's thumbnail: its label under the picture, its
  * accessible name, and whether it is the trial chosen for marking. `ref`
  * is the trial the thumbnail's numbers describe.
  */
final case class ThumbnailVM(
    trial: TrialKey,
    ref: StudioRef,
    label: String,
    accessible: String,
    marked: Boolean
) derives CanEqual

/** A recorded correction rule and its Remove button's accessible name. */
final case class RuleVM(index: Int, text: String, remove: String) derives CanEqual

final case class ChoiceVM[A](value: A, label: String, selected: Boolean) derives CanEqual

/** The open marking form. */
final case class OrientationVM(
    title: String,
    fixes: Vector[ChoiceVM[OrientationFix]],
    scopes: Vector[ChoiceVM[OrientationScope]],
    record: String,
    cancel: String
) derives CanEqual

/** A count of records outside a frame: its title, its value, what the count
  * means, and `ref`, the eyes4s tally the value is (none before it loads).
  */
final case class CountVM(
    title: String,
    value: Option[String],
    note: String,
    ref: Option[StudioRef]
) derives CanEqual

/** Everything the geometry panel shows, in the board's order. */
final case class GeometryPanelVM(
    title: String,
    kind: String,
    empty: Option[String],
    target: Option[String],
    facts: Vector[FactVM],
    fields: Vector[FieldVM],
    physical: Vector[(String, String)],
    physicalNote: String,
    example: Vector[ExampleRowVM],
    exampleRef: Option[StudioRef],
    placementTitle: String,
    placementNote: String,
    thumbnails: Vector[ThumbnailVM],
    positionsNote: Option[String],
    densityTitle: String,
    densityCaption: String,
    densityAccessible: String,
    mark: String,
    canMark: Boolean,
    markNote: Option[String],
    orientation: Option[OrientationVM],
    rulesTitle: String,
    rulesNote: String,
    rules: Vector[RuleVM],
    rulesEmpty: Option[String],
    policyTitle: String,
    policies: Vector[ChoiceVM[OffScreenChoice]],
    policyNote: String,
    outsideWindow: CountVM,
    outsideScreen: CountVM,
    countsSource: String,
    problem: Option[String]
) derives CanEqual

object GeometryPanelVM:
  import GeometryTextId.*

  private def t(id: GeometryTextId, args: String*): String = GeometryText(id, args*)

  /** A pixel coordinate as the boards write it: whole pixels without a
    * decimal or grouping ("1148"), others to one place ("−40.5").
    */
  def px(v: Double): String =
    if v.isWhole && v.abs < 1e12 then
      if v < 0 then Format.Minus + (-v.toLong).toString else v.toLong.toString
    else Format.decimal(v, 1)

  /** Degrees to one place with an explicit sign ("+5.4", "−2.4"). */
  def deg(v: Double): String = Format.signed(v, 1)

  def trialLabel(trial: TrialKey): String = s"${trial.participant} · ${trial.trial}"

  def fieldLabel(f: GeometryField): String = t(f match
    case GeometryField.ScreenWidth     => FieldScreenWidth
    case GeometryField.ScreenHeight    => FieldScreenHeight
    case GeometryField.ImageLeft       => FieldImageLeft
    case GeometryField.ImageTop        => FieldImageTop
    case GeometryField.ImageWidth      => FieldImageWidth
    case GeometryField.ImageHeight     => FieldImageHeight
    case GeometryField.PixelsPerDegree => FieldPpd)

  def correctionText(c: CoordinateCorrection): String = c match
    case CoordinateCorrection.FlipX        => t(CorrectionFlipX)
    case CoordinateCorrection.FlipY        => t(CorrectionFlipY)
    case CoordinateCorrection.Translate(o) => t(CorrectionTranslate, px(o.dx), px(o.dy))

  def targetText(target: CorrectionTarget): String = target match
    case CorrectionTarget.AllTrials      => t(TargetAll)
    case CorrectionTarget.Participant(p) => t(TargetParticipant, p.value)
    case CorrectionTarget.Trial(k)       => t(TargetTrial, trialLabel(k))

  def ruleText(index: Int, rule: CorrectionRule): String =
    t(RuleText, (index + 1).toString, correctionText(rule.correction), targetText(rule.target))

  /** The panel's view-model; `pictures` are the latest drawn for it, if any. */
  def of(
      panel: GeometryPanel,
      model: AppModel,
      pictures: Option[GeometryPictures]
  ): GeometryPanelVM =
    val spec   = panel.shown.flatMap(s => model.document.dataset(s.id))
    val g      = spec.map(_.geometry)
    val next   = GeometryPanel.nextRevision(model.document).label
    val target = spec.map { s =>
      s.decision match
        case AdmissionDecision.Pending        => t(EditsPending, s.id.label)
        case AdmissionDecision.Verifying(_)   => t(Verifying, s.id.label, next)
        case AdmissionDecision.Admitted(_, _) => t(ReadmitsAs, s.id.label, next)
    }
    val facts = g.toVector.flatMap { g =>
      Vector(
        FactVM(t(GazeCoordinates), t(GazeCoordinatesValue), mono = false, None),
        FactVM(
          t(ScreenLabel),
          t(ScreenValue, g.screen.width.toString, g.screen.height.toString),
          mono = true,
          None
        ),
        FactVM(
          t(PlacementLabel),
          t(
            PlacementValue,
            g.image.width.toString,
            g.image.height.toString,
            g.image.left.toString,
            g.image.top.toString
          ),
          mono = true,
          None
        ),
        FactVM(t(WindowLabel), t(WindowValue), mono = false, None),
        FactVM(t(PpdLabel), px(g.pixelsPerDegree.value), mono = true, Some(t(PpdDeclared)))
      )
    }
    val fields = spec.toVector.flatMap(_ =>
      GeometryField.values.toVector.map(f => FieldVM(f, fieldLabel(f), panel.fields.field(f)))
    )
    // The pictures on the canvases, until their redraw replaces them: the
    // labels always describe what is drawn. None before the records are read.
    val shownPictures = pictures.filter(p =>
      panel.positions.toOption.isDefined && panel.positionsKey.exists(
        PositionsKey.same(_, p.key.positions)
      )
    )
    val example     = shownPictures.flatMap(_.example)
    val exampleRows = example.toVector.flatMap { e =>
      val raw = ExampleRowVM(
        t(ExampleRaw, Format.count(e.record.toLong)),
        t(ExampleRawValue, px(e.rawX), px(e.rawY))
      )
      val corrected = e.rule.map((i, r) =>
        ExampleRowVM(
          t(ExampleCorrected, (i + 1).toString),
          t(
            ExampleCorrectedValue,
            px(e.correctedX),
            px(e.correctedY),
            correctionText(r.correction)
          )
        )
      )
      val image = ExampleRowVM(
        t(ExampleImage),
        (e.place, e.degrees) match
          case (MarkPlace.Inside, Some((dx, dy))) =>
            t(ExampleImageValue, px(e.imageX), px(e.imageY), deg(dx), deg(dy))
          case (MarkPlace.Inside, None) => t(ExampleNoDegrees, px(e.imageX), px(e.imageY))
          case _                        => t(ExampleImageOutside, px(e.imageX), px(e.imageY))
      )
      Vector(raw) ++ corrected ++ Vector(
        image,
        ExampleRowVM(t(ExampleDegrees), t(ExampleDegreesValue))
      )
    }
    val exampleRef = example.flatMap(e =>
      RecordNumber
        .of(e.record)
        .toOption
        .map(n => StudioRef.SourceRecord(e.trial, None, SourceRole.Fixations, n))
    )
    val thumbnails = shownPictures.toVector.flatMap(_.thumbnails).map { p =>
      val name  = trialLabel(p.trial)
      val label =
        if p.outsideScreen > 0 then
          t(ThumbOffScreen, name, p.outsideScreen.toString, p.records.toString)
        else if p.outsideWindow > 0 then
          t(ThumbOutside, name, p.outsideWindow.toString, p.records.toString)
        else t(ThumbInside, name, p.records.toString)
      val marked     = panel.marked.contains(p.trial)
      val accessible = t(
        ThumbAccessible,
        name,
        p.records.toString,
        p.outsideWindow.toString,
        p.outsideScreen.toString
      )
      ThumbnailVM(
        p.trial,
        StudioRef.Trial(p.trial),
        label,
        if marked then t(ThumbMarked, accessible) else accessible,
        marked
      )
    }
    val source        = spec.flatMap(_.sources.fixations).map(_.path.value).getOrElse("")
    val positionsNote = panel.positions match
      case Loading.Waiting                           => Some(t(PositionsWaiting, source))
      case Loading.Failed(why)                       => Some(t(PositionsFailed, why))
      case Loading.Ready(ps) if ps.unplaced.nonEmpty =>
        Some(t(PositionsUnplaced, Format.count(ps.unplaced.size.toLong)))
      case _ => None
    val records = panel.positions.toOption.map(p => Format.count(p.positions.size.toLong))
    val rules   = spec.toVector.flatMap(_.admission.corrections)
    val caption = records.fold(t(AllTrialsCaption, "—"))(n =>
      if rules.isEmpty then t(AllTrialsCaption, n)
      else t(AllTrialsCorrected, n, rules.size.toString)
    )
    val policy      = spec.map(_.admission.offScreen).getOrElse(OffScreenChoice.ExcludeRecord)
    val orientation = panel.orientation.map { f =>
      OrientationVM(
        t(OrientationTitle, trialLabel(f.trial)),
        Vector(
          ChoiceVM(OrientationFix.FlipX, t(FixX), f.fix == OrientationFix.FlipX),
          ChoiceVM(OrientationFix.FlipY, t(FixY), f.fix == OrientationFix.FlipY)
        ),
        Vector(
          ChoiceVM(
            OrientationScope.ThisTrial,
            t(ScopeTrial),
            f.scope == OrientationScope.ThisTrial
          ),
          ChoiceVM(
            OrientationScope.ThisParticipant,
            t(ScopeParticipant, f.trial.participant),
            f.scope == OrientationScope.ThisParticipant
          )
        ),
        t(RecordCorrection),
        t(Cancel)
      )
    }
    val (window, screen, from) = counts(panel, model, spec)
    GeometryPanelVM(
      title = t(Title),
      kind = ChangeKind.DatasetReadmit.label,
      empty = Option.when(spec.isEmpty)(t(NoDataset)),
      target = target,
      facts = facts,
      fields = fields,
      physical =
        Vector(t(ViewingDistance) -> t(NotRecorded), t(PhysicalWidth) -> t(NotRecorded)),
      physicalNote = t(PhysicalNote),
      example = exampleRows,
      exampleRef = exampleRef,
      placementTitle = t(CheckPlacement),
      placementNote = t(CheckPlacementNote),
      thumbnails = thumbnails,
      positionsNote = positionsNote,
      densityTitle = t(AllTrials),
      densityCaption = caption,
      densityAccessible = t(DensityAccessible, records.getOrElse("—")),
      mark = t(MarkOrientation),
      canMark = spec.isDefined && panel.marked.isDefined && panel.orientation.isEmpty,
      markNote = Option.when(spec.isDefined && panel.marked.isEmpty)(t(MarkNeedsTrial)),
      orientation = orientation,
      rulesTitle = t(RulesTitle),
      rulesNote = t(RulesNote),
      rules = rules.zipWithIndex.map((r, i) =>
        RuleVM(i, ruleText(i, r), t(RuleRemove, (i + 1).toString))
      ),
      rulesEmpty = Option.when(rules.isEmpty)(t(RulesEmpty)),
      policyTitle = t(PolicyTitle),
      policies = Vector(
        ChoiceVM(
          OffScreenChoice.ExcludeRecord,
          t(PolicyExclude),
          policy == OffScreenChoice.ExcludeRecord
        ),
        ChoiceVM(
          OffScreenChoice.QuarantineTrial,
          t(PolicyQuarantine),
          policy == OffScreenChoice.QuarantineTrial
        )
      ),
      policyNote = policy match
        case OffScreenChoice.ExcludeRecord   => t(PolicyExcludeNote)
        case OffScreenChoice.QuarantineTrial => t(PolicyQuarantineNote)
      ,
      outsideWindow = window,
      outsideScreen = screen,
      countsSource = from,
      problem = panel.problem
    )

  /** "543 of 11,520 records · 409 trials": eyes4s's window totals, outside
    * the image frame and outside the screen apart, over its count of source
    * records.
    */
  private def counts(
      panel: GeometryPanel,
      model: AppModel,
      spec: Option[DatasetRevisionSpec]
  ): (CountVM, CountVM, String) =
    val summary: Option[AdmissionSummary] = panel.counts.toOption
    // The note describes the counts shown, so it takes the policy of the
    // revision they were admitted under (a draft's parent, until the draft's own).
    val counted    = summary.flatMap(s => model.document.dataset(s.dataset)).orElse(spec)
    val policy     = counted.map(_.admission.offScreen).getOrElse(OffScreenChoice.ExcludeRecord)
    val screenNote = policy match
      case OffScreenChoice.ExcludeRecord   => t(OutsideScreenExcluded)
      case OffScreenChoice.QuarantineTrial => t(OutsideScreenQuarantined)
    def value(n: Int, of: Int, trials: Int) =
      t(
        CountValue,
        Format.count(n.toLong),
        Format.count(of.toLong),
        Format.count(trials.toLong)
      )
    val window = CountVM(
      t(OutsideWindowTitle),
      summary.map { s =>
        value(
          s.window.outsideWindow,
          s.window.sourceRecords.getOrElse(s.window.total),
          s.window.trialsOutsideWindow
        )
      },
      t(OutsideWindowNote),
      summary.map(s => StudioRef.WindowTally(s.dataset, TallyRegion.OutsideWindow))
    )
    val screen = CountVM(
      t(OutsideScreenTitle),
      summary.map { s =>
        value(
          s.window.outsideScreen,
          s.window.sourceRecords.getOrElse(s.window.total),
          s.window.trialsOutsideScreen
        )
      },
      screenNote,
      summary.map(s => StudioRef.WindowTally(s.dataset, TallyRegion.OutsideScreen))
    )
    val label = spec.fold("")(_.id.label)
    val from  = panel.counts match
      case Loading.Ready(s) =>
        if spec.exists(d => d.id != s.dataset && !d.decision.isAdmitted) then
          t(CountsPending, s.dataset.label, label)
        else t(CountsFrom, s.dataset.label)
      case Loading.Failed(why) => t(CountsFailed, label, why)
      case _                   => t(CountsWaiting)
    (window, screen, from)
