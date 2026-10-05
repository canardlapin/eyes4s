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

package eyes4s.studio.app.figures

import eyes4s.studio.app.admission.AdmissionLedgerVM
import eyes4s.studio.app.text.Format
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.*
import eyes4s.kernel.Span
import eyes4s.plan.WindowTally
import eyes4s.studio.core.engine.StudioBuild
import eyes4s.studio.core.figures.{FigureSource, MethodsFacts}

/** Where a value in the methods text comes from: the dataset, its admission,
  * the analysis recipe, the run's result, the reporting spec or the build.
  * A methods text states no number that is not one of these.
  */
enum MethodsSlot derives CanEqual:
  case SourceFile(role: SourceRole)
  case Dataset, FixationRecords, InventoryTrials, Admitted, Quarantined, NoFixations, Absent
  case QuarantineCause(code: String)
  case ImageFrame, Screen, AnalysisWindow
  case OutsideWindow, TalliedRecords, OutsideWindowShare, OutsideWindowTrials
  case OffWindowPolicy, OutsideScreen, OutsideScreenTrials, OffScreenPolicy
  case PixelsPerDegree, InitialFixations, Weighting, Grid, Cell, Scales
  case Phases, Method, MatchedRule, ControlRule
  case Queries, Eligible, Controls, ControlQueries
  case FailurePolicy, Contributing, Failed, NoMatch, NotAdmitted
  case FailureCause(code: String)
  case ReportingWeight, GroupBy, Filter, GroupN, PairedN, GroupRange
  case MinimumPerGroup, SmallestGroups
  case Analysis, Run, Build

/** One piece of a methods sentence: fixed words, or a value with its source. */
enum MethodsToken derives CanEqual:
  case Words(words: String)
  case Fact(slot: MethodsSlot, shown: String)

  def text: String = this match
    case Words(w)   => w
    case Fact(_, s) => s

final case class MethodsSentence(tokens: Vector[MethodsToken]) derives CanEqual:
  def text: String = tokens.map(_.text).mkString

/** The generated methods text of one figure (ticket S9.4), sentence by
  * sentence, with the run and reporting spec it cites once.
  */
final case class GeneratedMethods(
    run: RunId,
    reporting: String,
    sentences: Vector[MethodsSentence]
) derives CanEqual:
  def tokens: Vector[MethodsToken] = sentences.flatMap(_.tokens)
  def text: String                 = sentences.map(_.text).mkString(" ")

/** Why a figure's methods could not be generated. Every case names its
  * operands.
  */
enum MethodsError derives CanEqual:
  /** The result summary or the facts were read for another run. */
  case OtherRun(expected: RunId, found: RunId, what: String)

  /** The admission summary is of another dataset revision. */
  case OtherDataset(expected: DatasetRevision, found: DatasetRevision)

  def message: String = this match
    case OtherRun(expected, found, what) =>
      s"The methods of ${expected.label} were given the $what of ${found.label}."
    case OtherDataset(expected, found) =>
      s"The methods of dataset ${expected.label} were given the admission of ${found.label}."

/** Generates a figure's methods text (ticket S9.4; Figures.dc.html,
  * methods.md) from its bound run: the dataset revision and its admission,
  * the analysis recipe, the result summary, the reporting spec and the
  * build. Every number is a [[MethodsToken.Fact]].
  *
  * The plan's own methods phrase (`eyes4s.plan.StudyText.methods`) needs an
  * eyes4s `StudyPlan`, which Studio builds only on the real backend (S3.7),
  * and its typed fact slots are CR6d; until then the wording follows it
  * from the Studio recipe.
  */
object MethodsText:
  import MethodsToken.*
  import MethodsSlot as S

  private type Part = String | MethodsToken

  private def sentence(parts: Part*): MethodsSentence =
    MethodsSentence(
      parts.toVector
        .map {
          case t: MethodsToken => t
          case w: String       => Words(w)
        }
        .filter(_.text.nonEmpty)
    )

  private def n(slot: MethodsSlot, value: Long): MethodsToken = Fact(slot, Format.count(value))

  /** "a, b and c". */
  private def list(items: Vector[String]): String =
    if items.size < 2 then items.mkString else s"${items.init.mkString(", ")} and ${items.last}"

  /** `2` not `2.0`, as the document renders recipe values. */
  private def plain(value: Double): String =
    if value == math.rint(value) && math.abs(value) < 1e15 then value.toLong.toString
    else value.toString

  private def degrees(value: Double): String = s"${plain(value)}°"

  private def size(width: Double, height: Double): String =
    s"${plain(width)} × ${plain(height)}"

  /** "cosine similarity" for cosine; any other method by its definition. */
  def methodName(method: MethodSpec): String = method.definition.name match
    case "eyes4s.cosine" => "cosine similarity"
    case _               => method.render

  def generate(
      source: FigureSource,
      result: ResultSummary,
      facts: MethodsFacts
  ): Either[MethodsError, GeneratedMethods] =
    val run     = source.run.id
    val dataset = source.bound.dataset
    if result.run != run then Left(MethodsError.OtherRun(run, result.run, "result summary"))
    else if facts.run != run then Left(MethodsError.OtherRun(run, facts.run, "query facts"))
    else if facts.admission.dataset != dataset.id then
      Left(MethodsError.OtherDataset(dataset.id, facts.admission.dataset))
    else
      Right(
        GeneratedMethods(
          run,
          source.reporting.name,
          Vector(admission(dataset, facts.admission)) ++
            frame(source, facts.admission.window) ++
            Vector(offScreen(dataset, facts.admission.window)) ++
            angular(source) ++
            Vector(initial(source.bound.analysis.recipe), maps(source)) ++
            Vector(comparison(source, result, facts), outcomes(source, result, facts)) ++
            reporting(source, result) ++
            Vector(
              sentence("D measures spatial correspondence, not sequential replay."),
              sentence(
                "D does not separate participant-specific reinstatement from item-driven " +
                  "salience common to all viewers of that image, and may retain residual " +
                  "centre bias."
              ),
              sentence(
                "Analysis ",
                Fact(S.Analysis, source.bound.analysis.id.label),
                ", ",
                Fact(S.Run, run.label),
                "; ",
                Fact(S.Build, s"eyes4s ${StudioBuild.eyes4sBaseVersion}"),
                "."
              )
            )
        )
      )

  private def fileName(dataset: DatasetRevisionSpec, role: SourceRole): Option[MethodsToken] =
    dataset.sources.entries
      .find(_.role == role)
      .map(s => Fact(S.SourceFile(role), s.path.value.split('/').last))

  /** The causes a trial was quarantined for, by count, most first. */
  private def causes(a: AdmissionSummary): Vector[MethodsToken] =
    a.quarantined
      .map(q => (q.code, AdmissionLedgerVM.causeName(q.code), q.trials))
      .filter(_._3 > 0)
      .sortBy((_, name, trials) => (-trials, name))
      .map((code, name, trials) =>
        Fact(S.QuarantineCause(code), s"$name ${Format.count(trials)}")
      )

  private def admission(dataset: DatasetRevisionSpec, a: AdmissionSummary): MethodsSentence =
    // No fixations is a disposition of its own (Views.AdmissionSummary), not
    // a quarantine cause: said beside quarantine, never inside its count.
    val quarantined = a.quarantinedTrials
    val none    = Vector[Part](n(S.NoFixations, a.noFixations), " with no admitted fixations")
    val because = causes(a) match
      case Vector() => Vector.empty
      case cs       => Vector[Part](" (") ++ cs.flatMap(c => Vector[Part](", ", c)).tail :+ ")"
    val files =
      fileName(dataset, SourceRole.Fixations).toVector.flatMap(f => Vector[Part](f, ", "))
    val opening = Vector[Part]("Fixations (") ++ files ++ Vector[Part](
      n(S.FixationRecords, a.fixationRecords),
      " records; dataset ",
      Fact(S.Dataset, dataset.id.label),
      ") were admitted per trial"
    )
    a.inventory match
      case InventoryJoin.Joined(trials, absent) =>
        val inventory =
          fileName(dataset, SourceRole.Trials).toVector.flatMap(f => Vector[Part](f, " "))
        sentence(
          (opening ++ Vector[Part](" against the ") ++ inventory ++ Vector[Part](
            "inventory of ",
            n(S.InventoryTrials, trials),
            " trials: ",
            n(S.Admitted, a.admitted),
            " admitted, ",
            n(S.Quarantined, quarantined),
            " quarantined"
          ) ++ because ++ Vector[Part](", ") ++ none ++ Vector[Part](
            " and ",
            n(S.Absent, absent),
            " absent (no fixation records)."
          ))*
        )
      case InventoryJoin.Undeclared =>
        sentence(
          (opening ++ Vector[Part](
            ": ",
            n(S.Admitted, a.admitted),
            " admitted, ",
            n(S.Quarantined, quarantined),
            " quarantined"
          ) ++ because ++ Vector[Part](" and ") ++ none ++ Vector[Part](
            "; no trial inventory was declared, so absent trials were not counted."
          ))*
        )

  /** Whether the analysis window is the image frame on the screen. */
  private def isImageFrame(w: AnalysisWindow, g: Geometry): Boolean =
    w.xMin == g.image.left && w.yMin == g.image.top &&
      w.xMax == g.image.left + g.image.width && w.yMax == g.image.top + g.image.height

  private def windowName(source: FigureSource): String =
    source.bound.analysis.recipe.window match
      case Some(w) if isImageFrame(w, source.bound.dataset.geometry) => "frame"
      case Some(_)                                                   => "window"
      case None                                                      => "screen"

  private def frame(source: FigureSource, totals: WindowTotals): Vector[MethodsSentence] =
    val g      = source.bound.dataset.geometry
    val screen = Fact(S.Screen, size(g.screen.width, g.screen.height))
    source.bound.analysis.recipe.window match
      case None =>
        Vector(sentence("Positions were analysed on the whole ", screen, " screen."))
      case Some(w) =>
        val where =
          if isImageFrame(w, g) then
            Vector[Part](
              "Positions were analysed in the ",
              Fact(S.ImageFrame, size(g.image.width, g.image.height)),
              " image frame placed in a ",
              screen,
              " screen; "
            )
          else
            Vector[Part](
              "Positions were analysed in the analysis window ",
              Fact(S.AnalysisWindow, w.render),
              " of a ",
              screen,
              " screen; "
            )
        // eyes4s's own share of fixation duration outside the window
        // (WindowTally.outsideWindowShare), rebuilt from the served totals;
        // undefined when the fixations have no duration, and then not said.
        val share = WindowTally
          .of(
            totals.outsideScreen,
            totals.outsideWindow,
            totals.total,
            Span.micros(totals.outsideScreenMicros),
            Span.micros(totals.outsideWindowMicros),
            Span.micros(totals.totalMicros)
          )
          .toOption
          .flatMap(_.outsideWindowShare)
          .toVector
          .flatMap(s =>
            Vector[Part](
              " (",
              Fact(S.OutsideWindowShare, s"${Format.decimal(100.0 * s, 1)}%"),
              " of their fixation duration)"
            )
          )
        val fate = source.bound.analysis.recipe.offWindow match
          case Some(OffWindowChoice.Exclude) =>
            Vector[Part](
              " and were ",
              Fact(S.OffWindowPolicy, "excluded from the maps"),
              "; they are reported per trial."
            )
          case Some(OffWindowChoice.FailTrial) =>
            Vector[Part](" and ", Fact(S.OffWindowPolicy, "failed their trial"), ".")
          case None => Vector[Part](".")
        Vector(
          sentence(
            (where ++ Vector[Part](
              n(S.OutsideWindow, totals.outsideWindow),
              " of the ",
              n(S.TalliedRecords, totals.total),
              " fixation records of admitted trials"
            ) ++ share ++ Vector[Part](
              ", in ",
              n(S.OutsideWindowTrials, totals.trialsOutsideWindow),
              s" trials, fell outside the ${windowName(source)}"
            ) ++ fate)*
          )
        )

  /** The off-screen policy (eyes4s `OffScreenPolicy`) and what it applied to. */
  private def offScreen(dataset: DatasetRevisionSpec, totals: WindowTotals): MethodsSentence =
    val policy = dataset.admission.offScreen match
      case OffScreenChoice.ExcludeRecord   => "exclude the record"
      case OffScreenChoice.QuarantineTrial => "quarantine the trial"
    if totals.outsideScreen == 0 then
      sentence(
        "No fixation record lay outside the screen (off-screen policy: ",
        Fact(S.OffScreenPolicy, policy),
        ")."
      )
    else
      sentence(
        n(S.OutsideScreen, totals.outsideScreen),
        " fixation records, in ",
        n(S.OutsideScreenTrials, totals.trialsOutsideScreen),
        " trials, lay outside the screen (off-screen policy: ",
        Fact(S.OffScreenPolicy, policy),
        ")."
      )

  private def angular(source: FigureSource): Vector[MethodsSentence] =
    val centre = windowName(source) match
      case "frame"  => "image"
      case "window" => "analysis window"
      case _        => "screen"
    source.bound.analysis.recipe.angularScale.toVector.map(ppd =>
      sentence(
        s"Degrees are measured from the $centre centre (x right, y up) using a declared, " +
          "uncalibrated ",
        Fact(S.PixelsPerDegree, s"${plain(ppd.value)} px/°"),
        " with linear conversion."
      )
    )

  private def initial(recipe: Recipe): MethodsSentence = recipe.initialFixations match
    case InitialFixationChoice.KeepAll =>
      sentence(
        "The initial-fixation policy was ",
        Fact(S.InitialFixations, "Keep all"),
        ": every fixation, including the first, was kept."
      )
    case InitialFixationChoice.DropFirst =>
      sentence(
        "The initial-fixation policy was ",
        Fact(S.InitialFixations, "Drop first"),
        ": the first fixation of every trial was left out."
      )
    case InitialFixationChoice.DropLeadingNearCross(r) =>
      sentence(
        "The initial-fixation policy left out fixations within ",
        Fact(S.InitialFixations, degrees(r.degrees)),
        " of the fixation cross before the first saccade."
      )

  /** The width and height of one grid cell, in pixels: recipe arithmetic
    * Studio does until CR6d's plan descriptors serve the cell size
    * (bd-01M3DH0S5ZKFCN47MHDM0XWW10).
    */
  private def cell(source: FigureSource): (Double, Double) =
    val recipe = source.bound.analysis.recipe
    val screen = source.bound.dataset.geometry.screen
    val (w, h) = recipe.window.fold((screen.width.toDouble, screen.height.toDouble))(win =>
      (win.xMax - win.xMin, win.yMax - win.yMin)
    )
    (w / recipe.grid.columns, h / recipe.grid.rows)

  private def maps(source: FigureSource): MethodsSentence =
    val recipe   = source.bound.analysis.recipe
    val (cx, cy) = cell(source)
    val cellText = recipe.angularScale match
      case Some(ppd) =>
        val (x, y) = (Format.decimal(cx / ppd.value, 2), Format.decimal(cy / ppd.value, 2))
        if x == y then s"$x°" else s"$x × $y°"
      case None =>
        if cx == cy then s"${plain(cx)} px" else s"${size(cx, cy)} px"
    val weighting = recipe.weighting match
      case WeightChoice.Duration => "duration-weighted"
      case WeightChoice.Uniform  => "unweighted (count)"
    sentence(
      "Each trial was represented as a ",
      Fact(S.Weighting, weighting),
      " fixation density map on a ",
      Fact(S.Grid, size(recipe.grid.columns, recipe.grid.rows)),
      " grid (cell ",
      Fact(S.Cell, cellText),
      "), smoothed with Gaussian kernels at σ = ",
      Fact(S.Scales, list(recipe.scales.values.map(s => degrees(s.degrees)))),
      ", declared before the run."
    )

  private def comparison(
      source: FigureSource,
      result: ResultSummary,
      facts: MethodsFacts
  ): MethodsSentence =
    val recipe    = source.bound.analysis.recipe
    val focal     = recipe.phases.focal.label.toLowerCase
    val reference = recipe.phases.reference.label.toLowerCase
    val matched   = recipe.matched match
      case MatchedChoice.RequireOne     => s"the one matched $reference trial"
      case MatchedChoice.SameOccurrence =>
        s"the matched $reference trial of the same occurrence"
      case MatchedChoice.Select(pick) =>
        s"the ${pick.render} $reference presentation of the same item"
      case MatchedChoice.MeanOfAll =>
        s"every $reference presentation of the same item, averaged"
    val controls = recipe.controls match
      case ControlChoice.SameSelection =>
        s"every admitted $reference trial of another item from the same participant"
      case ControlChoice.AllOccurrences =>
        s"every admitted $reference presentation of every other item from the same participant"
    val tally = facts.controls.mode.toVector.flatMap { mode =>
      Vector[Part](" (", n(S.Controls, mode), " per query") ++
        facts.controls.entries
          .filter(_._1 != mode)
          .flatMap((count, queries) =>
            Vector[Part](
              "; ",
              n(S.Controls, count),
              " for ",
              n(S.ControlQueries, queries),
              if queries == 1 then " query" else " queries"
            )
          ) :+ ")"
    }
    sentence(
      (Vector[Part](
        "Of ",
        n(S.Queries, result.contrasts.requested),
        " ",
        Fact(S.Phases, focal),
        " queries, ",
        n(S.Eligible, result.eligibleQueries),
        " were eligible; for each eligible query, ",
        Fact(S.Method, methodName(recipe.method)),
        " was computed to ",
        Fact(S.MatchedRule, matched),
        " (M) and to ",
        Fact(S.ControlRule, controls)
      ) ++ tally ++ Vector[Part]("; their mean is B, and D = M − B."))*
    )

  private def outcomes(
      source: FigureSource,
      result: ResultSummary,
      facts: MethodsFacts
  ): MethodsSentence =
    val c      = result.contrasts
    val policy = source.bound.analysis.recipe.failurePolicy match
      case FailureChoice.RequireAll =>
        Vector[Part]("A contrast ", Fact(S.FailurePolicy, "required all of its pairs"), ": ")
      case FailureChoice.SuccessfulOnly(m) =>
        Vector[Part](
          "A contrast used its successful pairs, ",
          Fact(S.FailurePolicy, s"at least ${m.value}"),
          ": "
        )
    val why = facts.failures match
      case Vector()          => Vector.empty
      case Vector((code, _)) =>
        Vector[Part](" (", Fact(S.FailureCause(code), code.split('.').last), ")")
      case all =>
        Vector[Part](" (") ++ all
          .map((code, k) =>
            Fact(S.FailureCause(code), s"${code.split('.').last} ${Format.count(k)}")
          )
          .flatMap(f => Vector[Part](", ", f))
          .tail :+ ")"
    sentence(
      (policy ++ Vector[Part](
        n(S.Contributing, c.contributing),
        " contributed, ",
        n(S.Failed, c.failed),
        " failed"
      ) ++ why ++ Vector[Part](
        ", ",
        n(S.NoMatch, c.noMatch),
        " had no matched trial and ",
        n(S.NotAdmitted, c.queryNotAdmitted),
        " queries were not admitted."
      ))*
    )

  private def reporting(source: FigureSource, result: ResultSummary): Vector[MethodsSentence] =
    val spec  = source.reporting
    val focal = source.bound.analysis.recipe.phases.focal.label.toLowerCase
    val means = spec.weighting match
      case ReportingWeight.ParticipantMeans =>
        Fact(
          S.ReportingWeight,
          "averaged within participant, then across participants with equal weight"
        )
      case ReportingWeight.PooledQueries =>
        Fact(S.ReportingWeight, "averaged over pooled queries")
    val groups = result.groups
    val sizes  =
      if groups.isEmpty then Vector.empty
      else if groups.map(_.n).distinct.size == 1 then
        Vector[Part]("n = ", n(S.GroupN, groups.head.n), " each")
      else
        Vector[Part]("n = ") ++ groups
          .map(g => Fact(S.GroupN, s"${Format.count(g.n)} ${g.label.label}"))
          .flatMap(f => Vector[Part](", ", f))
          .tail
    val paired =
      if groups.size > 1 then Vector[Part]("; paired n = ", n(S.PairedN, result.pairedN))
      else Vector.empty
    val split = spec.groupBy.toVector.flatMap(c =>
      Vector[Part](", separately by ", Fact(S.GroupBy, s"$focal ${c.label}"))
    )
    val averaged = sentence(
      (Vector[Part]("D was ", means) ++ split ++
        (if sizes.isEmpty then Vector[Part](".")
         else Vector[Part](" (") ++ sizes ++ paired :+ ")."))*
    )
    val filters = Option
      .when(spec.filters.nonEmpty)(
        sentence(
          "The report kept only queries ",
          Fact(
            S.Filter,
            list(spec.filters.map {
              case ReportingFilter.Keep(a, values) =>
                s"whose ${a.label} is ${list(values.values)}"
              case ReportingFilter.OutsideWindowAtMost(share) =>
                s"with at most ${Format.decimal(100 * share.value, 1)}% of their fixation " +
                  "duration outside the window"
            })
          ),
          "."
        )
      )
      .toVector
    val perGroup = Option
      .when(spec.groupBy.isDefined && groups.nonEmpty)(groupSizes(spec, result))
      .toVector
    Vector(averaged) ++ filters ++ perGroup

  /** The per-participant group sizes, the minimum the spec applies, and who
    * holds the smallest groups.
    */
  private def groupSizes(spec: ReportingSpec, result: ResultSummary): MethodsSentence =
    val range =
      Fact(
        S.GroupRange,
        s"${Format.count(result.groupNMinimum)}–${Format.count(result.groupNMaximum)}"
      )
    val smallest = for
      p <- result.participants
      g <- p.groups
      if g.n == result.groupNMinimum
    yield (p.participant, g.label.label)
    val who = smallest.map(_._2).distinct match
      case Vector()      => Vector.empty
      case Vector(label) =>
        Vector[Part](
          " (",
          Fact(S.SmallestGroups, list(smallest.map(_._1))),
          if smallest.size == 1 then " has " else " each have ",
          n(S.GroupRange, result.groupNMinimum),
          " ",
          Fact(S.SmallestGroups, label),
          if result.groupNMinimum == 1 then " query)" else " queries)"
        )
      case _ =>
        Vector[Part](
          " (",
          Fact(S.SmallestGroups, list(smallest.map((p, l) => s"$p $l"))),
          ": ",
          n(S.GroupRange, result.groupNMinimum),
          if result.groupNMinimum == 1 then " query each)" else " queries each)"
        )
    spec.minimumPerGroup match
      case None =>
        sentence(
          (Vector[Part](
            "Per participant, groups held ",
            range,
            " queries; no minimum per group was applied in this reporting spec"
          ) ++ who :+ ".")*
        )
      case Some(m) =>
        sentence(
          "Per participant, groups held ",
          range,
          " queries; groups with fewer than ",
          n(S.MinimumPerGroup, m.queries),
          " queries were left out of this reporting spec."
        )
