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

import eyes4s.codec.{StudyCodecs, StudyInputCodecs}
import eyes4s.core.Weight
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.{Deg, Px}
import eyes4s.plan.*
import eyes4s.results.*
import eyes4s.surface.EdgePolicy
import io.circe.{Json, Printer}

import java.math.RoundingMode
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.security.MessageDigest

/** The scores of the Eyes Studio acceptance fixture computed by eyes4s
  * (ticket S0.7b): `fixtures/studio-golden` read through `FixationCsv`
  * against its trials table, the study of FIXTURE.md run at its four scales,
  * and the participant summaries reduced by eyes4s-results' `Report`.
  *
  * The rendered `SCORES.json` is deterministic: keys sorted, rows in key
  * order, and every score rounded half-even to [[Decimals]] places, far
  * coarser than the last-place differences `math.exp` may show between
  * JVMs and processors, so a byte change on another JVM is very unlikely,
  * though not impossible (x86_64 is unverified). Only `generatedWith`
  * records the environment, and a
  * check renders the regenerated scores with the checked-in `generatedWith`,
  * so regeneration is compared byte for byte on any JVM.
  *
  * Run `sbt "ioJVM/Test/runMain eyes4s.io.StudioScoresMain"` to write it;
  * `StudioFixtureRealSuite` checks it.
  */
object StudioScores:

  /** Decimal places of every score in SCORES.json. */
  val Decimals: Int = 6

  val Format: String    = "eyes4s.studio-golden.scores"
  val Version: Int      = 1
  val Generator: String = "io/.jvm/src/test/scala/eyes4s/io/StudioScores.scala"

  /** The scales of FIXTURE.md, in degrees. */
  val Sigmas: Vector[Double] = Vector(0.5, 1.0, 2.0, 4.0)

  val Groups: Vector[String] = Vector("Remembered", "Forgotten")

  def directory: Path =
    Iterator
      .iterate(Paths.get("").toAbsolutePath)(_.getParent)
      .takeWhile(_ != null)
      .map(_.resolve("fixtures/studio-golden"))
      .find(Files.isDirectory(_))
      .getOrElse(throw new IllegalStateException("fixtures/studio-golden not found"))

  def scoresFile: Path = directory.resolve("SCORES.json")

  /** The fixture's tables as stored. */
  final case class Tables(fixations: Array[Byte], trials: Array[Byte]):
    def source: StudioFixture.Source =
      StudioFixture.Source(
        String(fixations, StandardCharsets.UTF_8),
        String(trials, StandardCharsets.UTF_8)
      )

  def tables: Tables =
    Tables(
      Files.readAllBytes(directory.resolve("fixations.csv")),
      Files.readAllBytes(directory.resolve("trials.csv"))
    )

  /** The environment a file was generated on. */
  def environment: Json =
    def prop(k: String) = Json.fromString(sys.props.getOrElse(k, "unknown"))
    Json.obj(
      "java.vendor"          -> prop("java.vendor"),
      "java.version"         -> prop("java.version"),
      "java.vm.name"         -> prop("java.vm.name"),
      "java.runtime.version" -> prop("java.runtime.version"),
      "os.arch"              -> prop("os.arch"),
      "os.name"              -> prop("os.name")
    )

  private type K = TrialKey

  private def get[E, A](what: String)(e: Either[E, A]): A =
    e.fold(error => throw new IllegalStateException(s"$what: $error"), identity)

  /** Everything the fixture study produces, read through public API. */
  final class Study(source: StudioFixture.Source):
    val screen = get("screen")(Frame.screen("studio-screen", 1920, 1080))
    val image  = get("image")(
      Subframe.centred(screen, FrameId("studio-image"), get("extent")(Extent.of[Px](1024, 768)))
    )
    val grid    = get("grid")(Grid.over(image.frame, 64, 48))
    val columns =
      get("columns")(TrialColumns.of("participant", "phase", "trial", Some("occurrence")))
    val table = get("table")(
      FixationTable.of(
        columns,
        "ordinal",
        "x",
        "y",
        TimeColumns("onset_ms", "duration_ms", TimestampUnit.Milliseconds),
        SampleCountRule.PositiveColumn("sample_count")
      )
    )
    val inventoryColumns = get("inventory columns")(
      TrialInventoryColumns.of(
        columns,
        Some("item"),
        Vector("display_kind", "image_file", "response").map(
          AttributeColumn(_, AttributeKind.Text)
        )
      )
    )
    val admission = get("admission")(
      FixationCsv.admitInventory(
        source.fixations,
        table,
        get("trials")(TrialInventory.read(source.trials, inventoryColumns)),
        screen
      )
    )
    val ledger = get("ledger")(
      FixationEvidence.ledger(
        "fixations.csv",
        "trials.csv",
        admission,
        AdmissionDecision.ReviewExclusions
      )
    )
    val inventory = ledger.inventory.getOrElse(throw new IllegalStateException("no inventory"))
    val input     = StudyInput(admission.fixations.accepted)
    val pxPerDeg  = get("angular scale")(LinearAngularScale.of(screen, 35))
    val scales: Vector[StudyScale[Px]] = Sigmas.map(s =>
      StudyScale.Angular(
        StudyEstimate.Gaussian[Deg](get("sigma")(Sigma.deg(s)), EdgePolicy.Truncate)
      )
    )
    val plan = get("plan")(
      StudyPlan.configure(
        input.reference,
        TrialKey.layout(TrialKeyDefinitions.trialLayout),
        get("geometry")(StudyGeometry.windowed(image, grid, OffWindowPolicy.Exclude)),
        "Retrieval",
        "Encoding",
        Weight.Duration,
        scales,
        Some(pxPerDeg),
        FailurePolicy.RequireAll,
        StudyMethod.cosine[Px](DefinitionId.cosine),
        ()
      )
    )
    val work        = get("prepare")(plan.prepare(input))
    val preview     = get("preview")(work.preview)
    val cardinality = get("cardinality")(work.matchedCardinality)
    // Why each query without a match has none, judged by eyes4s against the
    // trials table (the design before admission).
    val unmatched   = get("unmatched reasons")(work.unmatchedReasons(inventory))
    val studyCounts = get("counts")(work.counts)
    val result      = get("run")(work.run)

    val response = get("covariate name")(CovariateName.of("response"))
    val levels   = get("levels")(Levels.of(Groups))
    val schema   = get("schema")(
      CovariateSchema.of(Vector(Covariate(response, CovariateType.Categorical(levels))))
    )
    // The public `ReportSources.study` digests the result through its full
    // JSON encoding, which for this study does not fit in 10 GB (S0.7b
    // finding, bead bd-01M441B59K01XN3EVVCRMQVHDV). The harness reads the
    // result in process instead, through the package's trusted source, bound
    // to the plan and input digests. The result is not digested: its field
    // holds the digest of a fixed "unbound" marker, never another artifact's
    // digest, and nothing here checks it.
    val planDigest      = get("plan digest")(StudyCodecs.trialCosine[Px].codec.digest(plan))
    val inputDigest     = get("input digest")(StudyInputCodecs.trial[Px].input.digest(input))
    private val Unbound = sha256(
      "eyes4s.studio-golden: result digest not computed".getBytes(StandardCharsets.UTF_8)
    )
    private def binding(d: eyes4s.codec.CanonicalDigest[?], field: String) =
      get(s"$field digest")(BindingDigest.parse(field, d.sha256.hex))
    val covariates = get("covariates")(
      CovariateTable.forKeys(schema, inventory, plan.layout, input.trials.rows.map(_.key))
    )
    val reports = get("report source")(
      ReportSource.study(
        plan,
        input,
        result,
        Some(covariates),
        ReportBinding(
          binding(planDigest, "plan"),
          binding(inputDigest, "input"),
          get("unbound result")(BindingDigest.parse("result", Unbound)),
          Some(binding(inputDigest, "covariates"))
        )
      )
    )

    def pairs(s: DirectedPairSchedule[K, K]): Vector[(K, K)] =
      @annotation.tailrec
      def loop(c: PairCursor[K, K], acc: Vector[(K, K)]): Vector[(K, K)] =
        get("pairs")(c.advance(PairQuantum.default)) match
          case PairPage.More(ps, _, next) => loop(next, acc ++ ps.map(p => p.left -> p.right))
          case PairPage.Done(ps, _, _)    => acc ++ ps.map(p => p.left -> p.right)
      loop(s.start, Vector.empty)

    lazy val matched: Map[K, Vector[K]] =
      pairs(preview.matched).groupMap(_._1)(_._2)
    lazy val controls: Map[K, Int] =
      pairs(preview.controls).groupMapReduce(_._1)(_ => 1)(_ + _)

    def queries(scale: Int): QueryTable[K] = get(s"queries $scale")(reports.queries(scale))

    private val value = "value"

    def report(scale: Int, grouped: Boolean): Report[K] =
      val term = LevelTerm.Categorical(response, levels)
      val spec = get("spec")(
        ReportSpec.of(
          get("id")(ReportId.of(if grouped then s"by-response-$scale" else s"all-$scale")),
          scale,
          get("selection")(
            ReportSelection
              .of(Vector(Role.Matched, Role.Control, Role.Difference), Vector(value))
          ),
          groupBy = if grouped then Vector(Grouping.ByLevel(term)) else Vector.empty,
          contrast = Option.when(grouped)(LevelContrast(term, Groups(0), Groups(1)))
        )
      )
      get(s"report $scale")(Report.evaluate(spec, reports))

  // ---------------------------------------------------------------------------
  // Rendering
  // ---------------------------------------------------------------------------

  private val printer = Printer.spaces2.copy(sortKeys = true, dropNullValues = false)

  def number(v: Double): Json =
    Json.fromBigDecimal(BigDecimal(v).bigDecimal.setScale(Decimals, RoundingMode.HALF_EVEN))

  private def value(v: Value[Double]): Json = v match
    case Value.Present(d) => number(d)
    case Value.Missing(_) => Json.Null

  private def sigmaKey(s: Double): String =
    if s == math.rint(s) then s.toLong.toString else s.toString

  private def sha256(bytes: Array[Byte]): String =
    MessageDigest.getInstance("SHA-256").digest(bytes).map(b => f"${b & 0xff}%02x").mkString

  private def trialLabel(k: K): String = k.trial

  /** The JSON body: everything but `generatedWith`. */
  def body(tables: Tables): Json = body(tables, Study(tables.source))

  /** The JSON body of `s`, the study of `tables`. */
  def body(tables: Tables, s: Study): Json =
    val inv = s.inventory
    import s.{cardinality, preview}

    val retrieval = inv.trials.filter(_.identity.phase == "Retrieval")
    val focal     = preview.focalKeys
    // The status of each query is eyes4s's: no match from its unmatched
    // reasons, which the report table lists apart from its eligible queries,
    // and contributing or failed from its stored contrast row.
    val noMatch   = s.unmatched.reasons.map(_._1).toSet
    val tallies   = preview.windowTallies.collect { case (k, Right(t)) => k -> t }.toMap
    val summary   = preview.windowSummary
    val contrast0 = s.queries(0)
    if contrast0.unmatched.toSet != noMatch then
      throw new IllegalStateException(
        s"report table lists ${contrast0.unmatched.size} unmatched; reasons give ${noMatch.size}"
      )
    val status: Map[K, String] = contrast0.unmatched.map(_ -> "no-match").toMap ++
      contrast0.queries.map { q =>
        q.key -> (
          q.outcome(Role.Difference) match
            case RoleOutcome.Scored(_)       => "contributing"
            case RoleOutcome.Failed(code, _) => s"failed:${code.render}"
            case RoleOutcome.NotStored       => "not-stored"
        )
      }
    val statusCounts =
      status.values.map(_.takeWhile(_ != ':')).groupMapReduce(identity)(_ => 1)(_ + _)
    val quarantined = inv.quarantined
      .map(_.disposition match
        case TrialDisposition.Quarantined(cause) => cause.productPrefix
        case other                               => other.label)
      .groupMapReduce(identity)(_ => 1)(_ + _)
    val controlCounts =
      s.matched.keys.toVector
        .map(k => s.controls.getOrElse(k, 0))
        .groupMapReduce(identity)(_ => 1)(_ + _)

    val counts = Json.obj(
      "inventoryTrials"    -> Json.fromInt(inv.trials.size),
      "admittedTrials"     -> Json.fromInt(inv.admitted.size),
      "quarantinedTrials"  -> Json.fromInt(inv.quarantined.size),
      "quarantinedByCause" -> Json.fromFields(
        quarantined.toVector.sorted.map((k, v) => k -> Json.fromInt(v))
      ),
      "absentTrials"                -> Json.fromInt(inv.absent.size),
      "records"                     -> Json.fromInt(s.ledger.records.size),
      "admittedRecords"             -> Json.fromInt(s.ledger.admitted.size),
      "rejectedRecords"             -> Json.fromInt(s.ledger.rejected.size),
      "recordsOutsideWindow"        -> Json.fromInt(summary.outsideWindow),
      "trialsOutsideWindow"         -> Json.fromInt(summary.trialsOutsideWindow),
      "recordsOutsideScreen"        -> Json.fromInt(summary.outsideScreen),
      "items"                       -> Json.fromInt(inv.trials.flatMap(_.item).distinct.size),
      "requestedQueries"            -> Json.fromInt(retrieval.size),
      "queriesNotAdmitted"          -> Json.fromInt(retrieval.size - focal.size),
      "focalTrials"                 -> Json.fromInt(focal.size),
      "referenceTrials"             -> Json.fromInt(preview.referenceKeys.size),
      "noMatchQueries"              -> Json.fromInt(noMatch.size),
      "byDesignQueries"             -> Json.fromInt(s.unmatched.byDesign),
      "noMatchReferenceNotAdmitted" -> Json.fromInt(s.unmatched.notAdmitted),
      "noMatchReferenceNotPairable" -> Json.fromInt(s.unmatched.notPairable),
      "multipleMatchQueries"        -> Json.fromInt(cardinality.multiple.size),
      "failedQueries"               -> Json.fromInt(statusCounts.getOrElse("failed", 0)),
      "contributingQueries"         -> Json.fromInt(statusCounts.getOrElse("contributing", 0)),
      "eligibleQueries"             -> Json.fromLong(s.studyCounts.eligibleQueries),
      "queriesWithAMatch"           -> Json.fromInt(s.matched.size),
      "controlsPerQuery"            -> Json.fromFields(
        controlCounts.toVector.sorted.map((k, v) => k.toString -> Json.fromInt(v))
      ),
      // eyes4s StudyCounts: every scheduled pair row, failed outcomes included.
      "pairRowsPerScale"       -> Json.fromLong(s.studyCounts.pairRowsPerScale),
      "pairRowsAllScales"      -> Json.fromLong(s.studyCounts.totalPairs),
      "matchedPairsPerScale"   -> Json.fromLong(s.studyCounts.matched.eligiblePairs),
      "controlPairsPerScale"   -> Json.fromLong(s.studyCounts.controls.eligiblePairs),
      "mapsPerScale"           -> Json.fromLong(s.studyCounts.mapsPerScale),
      "candidatePairsPerScale" -> Json.fromLong(preview.matched.candidatePairCount)
    )

    val tables0 = Sigmas.indices.map(s.queries).toVector
    val byKey   = tables0.map(_.queries.map(q => q.key -> q).toMap)
    // A query without a match is not in the table; it has no score at any
    // scale, and its response is read from the covariate table directly.
    def response(k: K): Value[CovariateValue] =
      byKey(0).get(k).fold(s.covariates.value(k, s.response))(_.covariate(s.response))
    val noScore = Value.Missing(Absence.NotRecorded)
    val queries = focal.sorted(using s.plan.layout.ordering).map { k =>
      Json.obj(
        "participant" -> Json.fromString(k.participant),
        "trial"       -> Json.fromString(trialLabel(k)),
        "occurrence"  -> Json.fromInt(k.occurrence.value),
        "item"        -> Json.fromString(k.item),
        "response"    -> (response(k) match
          case Value.Present(CovariateValue.Level(l)) => Json.fromString(l)
          case Value.Present(other)                   => Json.fromString(other.toString)
          case Value.Missing(_)                       => Json.Null),
        "matched" -> Json.fromValues(
          s.matched.getOrElse(k, Vector.empty).map(r => Json.fromString(trialLabel(r)))
        ),
        "controls" -> s.matched
          .get(k)
          .fold(Json.Null)(_ => Json.fromInt(s.controls.getOrElse(k, 0))),
        "status"        -> Json.fromString(status.getOrElse(k, "absent-from-table")),
        "noMatchKind"   -> s.unmatched.reason(k).fold(Json.Null)(r => Json.fromString(r.code)),
        "outsideWindow" -> tallies
          .get(k)
          .fold(Json.Null)(t =>
            Json
              .obj("fixations" -> Json.fromInt(t.outsideWindow), "of" -> Json.fromInt(t.total))
          ),
        "scales" -> Json.fromFields(Sigmas.indices.map { i =>
          val q             = byKey(i).get(k)
          def role(r: Role) = value(q.fold(noScore)(_.outcome(r).value(0)))
          sigmaKey(Sigmas(i)) -> Json.obj(
            "M" -> role(Role.Matched),
            "B" -> role(Role.Control),
            "D" -> role(Role.Difference)
          )
        })
      )
    }
    val notAdmitted = retrieval
      .filterNot(t =>
        focal.exists(k =>
          k.participant == t.identity.participant && k.trial == t.identity.trial
        )
      )
      .map(t =>
        Json.obj(
          "participant" -> Json.fromString(t.identity.participant),
          "trial"       -> Json.fromString(t.identity.trial),
          "disposition" -> Json.fromString(t.disposition.label)
        )
      )
      .sortBy(j => j.noSpaces)

    def cellJson(c: Cell[K]): Json = Json.obj(
      "estimate"     -> value(c.estimate),
      "participants" -> Json.fromInt(c.participants),
      "queries"      -> Json.fromInt(c.queries),
      "failed"       -> Json.fromInt(c.failed)
    )
    val summaries = Sigmas.indices.map { i =>
      val all                                         = s.report(i, grouped = false)
      val grouped                                     = s.report(i, grouped = true)
      def cell(r: Report[K], g: GroupKey, role: Role) = r.cell(g, role, "value")
      val roles = Vector(Role.Matched -> "M", Role.Control -> "B", Role.Difference -> "D")
      val participants = {
        val per = roles.map { (role, name) =>
          name -> cell(all, GroupKey.all, role).fold(Map.empty[String, ParticipantValue])(
            _.perParticipant.map(p => p.participant -> p).toMap
          )
        }.toMap
        val byGroup = Groups.map { g =>
          g -> grouped.groups
            .find(_.levels.exists(_._2 == g))
            .flatMap(gk => cell(grouped, gk, Role.Difference))
            .fold(Map.empty[String, ParticipantValue])(
              _.perParticipant.map(p => p.participant -> p).toMap
            )
        }.toMap
        val names = (per.values.flatMap(_.keys) ++ byGroup.values.flatMap(
          _.keys
        )).toVector.distinct.sorted
        Json.fromFields(names.map { p =>
          p -> Json.obj(
            "queries"    -> per("D").get(p).fold(Json.fromInt(0))(v => Json.fromInt(v.queries)),
            "M"          -> per("M").get(p).fold(Json.Null)(v => value(v.value)),
            "B"          -> per("B").get(p).fold(Json.Null)(v => value(v.value)),
            "D"          -> per("D").get(p).fold(Json.Null)(v => value(v.value)),
            "byResponse" -> Json.fromFields(Groups.map { g =>
              g -> byGroup(g)
                .get(p)
                .fold(Json.obj("D" -> Json.Null, "queries" -> Json.fromInt(0)))(v =>
                  Json.obj("D" -> value(v.value), "queries" -> Json.fromInt(v.queries))
                )
            })
          )
        })
      }
      val groups = Json.fromFields(Groups.map { g =>
        g -> grouped.groups
          .find(_.levels.exists(_._2 == g))
          .flatMap(gk => cell(grouped, gk, Role.Difference))
          .fold(Json.Null)(cellJson)
      })
      val contrast = grouped.contrasts
        .find(c => c.role == Role.Difference)
        .fold(Json.Null)(c =>
          Json.obj(
            "estimate" -> value(c.estimate),
            "paired"   -> Json.fromInt(c.paired.size),
            "unpaired" -> Json.fromInt(c.unpaired.size)
          )
        )
      sigmaKey(Sigmas(i)) -> Json.obj(
        "grand" -> Json.fromFields(
          roles.map((role, name) =>
            name -> cell(all, GroupKey.all, role).fold(Json.Null)(cellJson)
          )
        ),
        "byResponse"               -> groups,
        "rememberedMinusForgotten" -> contrast,
        "participants"             -> participants
      )
    }

    Json.obj(
      "format"    -> Json.fromString(Format),
      "version"   -> Json.fromInt(Version),
      "generator" -> Json.fromString(Generator),
      "inputs"    -> Json.obj(
        "fixations.csv" -> Json.fromString(sha256(tables.fixations)),
        "trials.csv"    -> Json.fromString(sha256(tables.trials))
      ),
      // The recipe as the plan describes itself (the plan digest binds it),
      // with the scales in degrees and the report's reduction.
      "recipe" -> Json.obj(
        "description" -> Json.fromValues(
          s.plan.description.map((field, params) =>
            Json.obj(
              "field"  -> Json.fromString(field),
              "values" -> Json.fromValues(params.map(p => Json.fromString(p.render)))
            )
          )
        ),
        "scales"   -> Json.fromValues(Sigmas.map(number)),
        "reduce"   -> Json.fromString(ReducePolicy.default.toString),
        "decimals" -> Json.fromInt(Decimals)
      ),
      "plan"        -> Json.fromString(s.reports.binding.plan.hex),
      "input"       -> Json.fromString(s.reports.binding.input.hex),
      "counts"      -> counts,
      "queries"     -> Json.fromValues(queries),
      "notAdmitted" -> Json.fromValues(notAdmitted),
      "summaries"   -> Json.fromFields(summaries)
    )

  /** The file: `body` with `generatedWith`, printed with sorted keys. */
  def render(body: Json, generatedWith: Json): String =
    printer.print(body.deepMerge(Json.obj("generatedWith" -> generatedWith))) + "\n"

/** Writes `fixtures/studio-golden/SCORES.json` (S0.7b). Never hand-edit it. */
object StudioScoresMain:
  def main(args: Array[String]): Unit =
    val text =
      StudioScores.render(StudioScores.body(StudioScores.tables), StudioScores.environment)
    Files.writeString(StudioScores.scoresFile, text, StandardCharsets.UTF_8)
    println(s"wrote ${StudioScores.scoresFile} (${text.length} characters)")
