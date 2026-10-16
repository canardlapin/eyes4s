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
import eyes4s.studio.app.maps.{DensityBand, DensityCasing, DensityGround}
import eyes4s.studio.app.plot.RovingMove
import eyes4s.studio.app.text.{Format, GeometryText, GeometryTextId}
import eyes4s.studio.app.tokens.StageVariant
import eyes4s.studio.app.vm.{A11yRole, FocusStop}
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

/** A representative trial's thumbnail: the two caption lines under the
  * picture, the trial (`name`, "P05 · ret_04") and its count ("11 of 11
  * outside", or "Missing asset · forest-044.png" in the warning ink when
  * `assetMissing`), its accessible name, and whether it is the trial chosen
  * for marking. `ref` is the trial the thumbnail's numbers describe.
  */
final case class ThumbnailVM(
    trial: TrialKey,
    ref: Option[StudioRef],
    name: String,
    count: String,
    accessible: String,
    marked: Boolean,
    assetMissing: Boolean
) derives CanEqual

/** The all-trials density's key (DESIGN_SPEC section 15.1): its caption,
  * the four swatch labels, lowest level first, the stage the density is
  * drawn on, and the band each level's swatch wears there. Swatch `i` is
  * ramp stop `i`, opaque, over `stage`, so it takes the stage ground's rule
  * ([[DensityCasing]]): bordered 1 px by `bands(i)`, if any.
  */
final case class DensityKeyVM(
    caption: String,
    labels: Vector[String],
    stage: StageVariant,
    bands: Vector[Option[DensityBand]]
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
) derives CanEqual:
  /** The count's accessible name: its title and its value ("—" before it loads). */
  def accessible: String = s"$title: ${value.getOrElse("—")}"

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
    thumbnailsAccessible: String,
    positionsNote: Option[String],
    densityTitle: String,
    densityCaption: String,
    densityAccessible: String,
    densityKey: DensityKeyVM,
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

  /** A trial as the panel names it ("P05 · ret_04"). */
  def trialLabel(trial: TrialKey): String = t(ThumbTrial, trial.participant, trial.trial)

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

  /** The density levels of `steps` as closed count ranges, lowest first, and
    * the overflow level open: "1–3", "4–15", "16–63", "≥64" for a step of 4
    * (DESIGN_SPEC section 15.1). The key and the density's semantics text
    * both read them.
    */
  def densityLabels(steps: DensitySteps): Vector[String] =
    val starts = steps.starts
    starts
      .zip(starts.drop(1))
      .map((from, next) => t(DensityRange, Format.count(from), Format.count(next - 1))) :+
      t(DensityOverflow, Format.count(starts.last))

  /** The key of a density of `columns × rows` screen cells (dashes before
    * the grid is known) at `steps`, drawn on `stage`.
    */
  def densityKey(
      grid: Option[(Int, Int)],
      steps: DensitySteps,
      stage: StageVariant
  ): DensityKeyVM =
    val (c, r) =
      grid.fold(("—", "—"))((c, r) => (Format.count(c.toLong), Format.count(r.toLong)))
    DensityKeyVM(
      t(DensityKey, c, r),
      densityLabels(steps),
      stage,
      DensityCasing.levels(stage, DensityGround.Stage)
    )

  /** The document's stage as the renderer's token variant. */
  def stageOf(model: AppModel): StageVariant =
    model.document.presentation.stage match
      case StageAppearance.Dark  => StageVariant.Dark
      case StageAppearance.Mid   => StageVariant.Mid
      case StageAppearance.Light => StageVariant.Light

  /** A thumbnail's count line: a missing or unreadable asset first ("Missing
    * asset · forest-044.png", the Data board's caption), then off screen
    * before outside the frame, else the records inside, in the singular for
    * one record.
    */
  def thumbCount(p: ThumbnailPicture): String =
    p.frame match
      case FrameState.Missing(file)       => t(ThumbMissing, file.value)
      case FrameState.Unreadable(file, _) => t(ThumbUnreadable, file.value)
      case FrameState.Present             => recordsCount(p)

  private def recordsCount(p: ThumbnailPicture): String =
    def n(v: Int): String = Format.count(v.toLong)
    if p.outsideScreen > 0 then t(ThumbOffScreen, n(p.outsideScreen), n(p.records))
    else if p.outsideWindow > 0 then t(ThumbOutside, n(p.outsideWindow), n(p.records))
    else if p.records == 1 then t(ThumbInsideOne)
    else t(ThumbInside, n(p.records))

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
        case AdmissionDecision.Pending           => t(EditsPending, s.id.label)
        case AdmissionDecision.Verifying(_)      => t(Verifying, s.id.label, next)
        case AdmissionDecision.Admitted(_, _, _) => t(ReadmitsAs, s.id.label, next)
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
    // The pictures on the canvases, until their redraw replaces them (also
    // while a changed revision's placement is asked again): the labels always
    // describe what is drawn. None before the records are placed, and none
    // once the placement is refused: a refusal blanks them.
    val drawnFor = pictures.filter(p =>
      panel.placementKey.exists(PlacementKey.sameRecords(_, p.key.placement))
    )
    val shownPictures = panel.placement match
      case Loading.Failed(_) => None
      case _                 => drawnFor
    // Pictures of the same records under other rules or geometry: said to be
    // the previous placement's, and citing no tally of the edited revision.
    val stale = shownPictures.exists(p =>
      !panel.placementKey.exists(PlacementKey.same(_, p.key.placement))
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
    val thumbnails =
      shownPictures.toVector.flatMap(pic => pic.thumbnails.map(pic.key.dataset -> _)).map {
        (dataset, p) =>
          val name    = trialLabel(p.trial)
          val marked  = panel.marked.contains(p.trial)
          val window  = Format.count(p.outsideWindow.toLong)
          val screen  = Format.count(p.outsideScreen.toLong)
          val records =
            if p.records == 1 then t(ThumbRecordsOne, window, screen)
            else t(ThumbRecords, Format.count(p.records.toLong), window, screen)
          // A missing or unreadable asset is named first, as the caption names it.
          val accessible = p.frame match
            case FrameState.Present       => t(ThumbAccessible, name, records)
            case FrameState.Missing(file) =>
              t(ThumbAccessibleMissing, name, file.value, records)
            case FrameState.Unreadable(file, _) =>
              t(ThumbAccessibleUnreadable, name, file.value, records)
          ThumbnailVM(
            p.trial,
            // The thumbnail's counts are the backend's window tally of the
            // trial, unless they are the previous placement's.
            Option.when(!stale)(StudioRef.TrialPlacementTally(dataset, p.trial)),
            name,
            thumbCount(p),
            if marked then t(ThumbMarked, accessible) else accessible,
            marked,
            p.frame != FrameState.Present
          )
      }
    val source        = spec.flatMap(_.sources.fixations).map(_.path.value).getOrElse("")
    val positionsNote = panel.placement match
      case _ if stale                                 => Some(t(PositionsStale, source))
      case Loading.Waiting                            => Some(t(PositionsWaiting, source))
      case Loading.Failed(why)                        => Some(t(PositionsFailed, why))
      case Loading.Ready(ps) if ps.unplaced.size == 1 => Some(t(PositionsUnplacedOne))
      case Loading.Ready(ps) if ps.unplaced.nonEmpty  =>
        Some(t(PositionsUnplaced, Format.count(ps.unplaced.size.toLong)))
      case _ => None
    val records = panel.placement.toOption.map(p => Format.count(p.records.size.toLong))
    val rules   = spec.toVector.flatMap(_.admission.corrections)
    val caption = records.fold(t(AllTrialsCaption, "—"))(n =>
      if rules.isEmpty then t(AllTrialsCaption, n)
      else t(AllTrialsCorrected, n, rules.size.toString)
    )
    // The key labels the density drawn, at its own revision's levels; before
    // one is drawn, the default levels over the grid being placed.
    val drawnDensity = shownPictures.map(_.density)
    val densityKey   = GeometryPanelVM.densityKey(
      drawnDensity
        .map(d => d.columns -> d.rows)
        .orElse(panel.placement.toOption.map(p => p.density.columns -> p.density.rows)),
      drawnDensity.fold(DensitySteps.Default)(_.steps),
      stageOf(model)
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
      thumbnailsAccessible =
        if thumbnails.size == 1 then t(ThumbListOne)
        else t(ThumbList, Format.count(thumbnails.size.toLong)),
      positionsNote = positionsNote,
      densityTitle = t(AllTrials),
      densityCaption = caption,
      densityAccessible = t(DensityAccessible, records.getOrElse("—")),
      densityKey = densityKey,
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

  /** The panel's focus stops inside its pane, in the order Tab visits them
    * (S1.11; they follow the pane's stop): the typed fields when the panel
    * edits them itself (`fields`; in the window the import draft's geometry
    * form takes their place, with stops of its own), the thumbnails (one
    * list stop with a cursor, while any is shown; [[thumbnailHome]]), the
    * all-trials density, the marking button when it can mark, the open marking form (its
    * chosen fix and scope, then its two buttons), each recorded rule's
    * Remove, the chosen off-screen policy, and each count that opens its
    * tally. None while no revision is shown. The viewing distance and screen
    * width are not recorded anywhere, so they are never stops.
    */
  def focusStops(vm: GeometryPanelVM, fields: Boolean): Vector[FocusStop] =
    // Tab enters a radio group at its chosen radio.
    def chosen[A](choices: Vector[ChoiceVM[A]]): Vector[FocusStop] =
      choices
        .find(_.selected)
        .orElse(choices.headOption)
        .map(c => FocusStop(A11yRole.RadioButton, c.label))
        .toVector
    def button(name: String) = FocusStop(A11yRole.Button, name)
    if vm.empty.isDefined then Vector.empty
    else
      val typed =
        if fields then vm.fields.map(f => FocusStop(A11yRole.TextField, f.label))
        else Vector.empty
      val form = vm.orientation.toVector.flatMap(o =>
        chosen(o.fixes) ++ chosen(o.scopes) ++ Vector(button(o.record), button(o.cancel))
      )
      typed ++
        Option.when(vm.thumbnails.nonEmpty)(
          FocusStop(A11yRole.List, vm.thumbnailsAccessible)
        ) ++
        Vector(FocusStop(A11yRole.Plot, vm.densityAccessible)) ++
        Option.when(vm.canMark)(button(vm.mark)) ++
        form ++
        vm.rules.map(r => button(r.remove)) ++
        chosen(vm.policies) ++
        Vector(vm.outsideWindow, vm.outsideScreen)
          .filter(_.ref.isDefined)
          .map(c => button(c.accessible))

  /** The thumbnails' columns, as the board lays them out. */
  val ThumbnailColumns: Int = 2

  /** The thumbnail a focused strip puts its cursor on when it has none
    * (DESIGN_SPEC section 15, composite focus): the one chosen for marking,
    * else the first; none while no thumbnail is shown.
    */
  def thumbnailHome(thumbnails: Vector[ThumbnailVM]): Option[Int] =
    thumbnails.indexWhere(_.marked) match
      case -1 => Option.when(thumbnails.nonEmpty)(0)
      case i  => Some(i)

  /** Where `move` takes the strip's cursor from thumbnail `at` of `count`,
    * laid out in [[ThumbnailColumns]] columns in reading order: Left and
    * Right (and Previous and Next) step to the neighbour in that order, Up
    * and Down to the thumbnail a row above or below, which stays put when
    * there is none, Home and End to the first and the last. It never wraps.
    */
  def thumbnailMove(at: Int, move: RovingMove, count: Int): Int =
    if count <= 0 then at
    else
      val last       = count - 1
      val from       = at.max(0).min(last)
      def to(i: Int) = if i >= 0 && i <= last then i else from
      move match
        case RovingMove.Left | RovingMove.Previous => (from - 1).max(0)
        case RovingMove.Right | RovingMove.Next    => (from + 1).min(last)
        case RovingMove.Up                         => to(from - ThumbnailColumns)
        case RovingMove.Down                       => to(from + ThumbnailColumns)
        case RovingMove.First                      => 0
        case RovingMove.Last                       => last

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
