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

package eyes4s.studio.core.document

import eyes4s.studio.core.backend.{
  AnalysisRevision,
  DatasetRevision,
  JobId,
  RunId,
  StageKind,
  TrialKey
}
import eyes4s.codec.{CanonicalDigest, CodecError, SchemaLadder, VersionedCodec}
import io.circe.syntax.*
import io.circe.{Codec, Decoder, Encoder}

// ---------------------------------------------------------------------------
// Runs
// ---------------------------------------------------------------------------

/** What happened to a run, as the document records it. Whether a completed
  * run is current or stale is derived (S2.7) from the revisions it names,
  * never stored. A running run's job handle is session state, not science:
  * it lives in [[JobHandle]] beside the science.
  */
enum RunLifecycle derives CanEqual, Codec.AsObject:
  case Running
  case Completed
  case Cancelled(at: Option[StageKind])
  case Failed

/** The backend job executing a running run. Ephemeral: a job id means
  * nothing after the backend restarts, so it is kept out of the document's
  * science and its hash.
  */
final case class JobHandle(run: RunId, job: JobId) derives CanEqual, Codec.AsObject

/** One run of an analysis revision on a dataset revision, and the eyes4s
  * result archive it produced once completed.
  */
final case class RunRef(
    id: RunId,
    analysis: AnalysisRevision,
    dataset: DatasetRevision,
    state: RunLifecycle,
    archive: CoreBinding[ResultArchiveArtifact]
) derives CanEqual,
      Codec.AsObject:
  def label: String = s"${id.label} · ${analysis.label} · data ${dataset.label}"

// ---------------------------------------------------------------------------
// Reporting ("Reporting · no rerun"), mirroring UI-C's ReportSpec
// ---------------------------------------------------------------------------

/** A reporting spec's stable identity within a document. */
final case class ReportingId private (value: String) derives CanEqual

object ReportingId:
  def of(value: String): Either[DocumentError, ReportingId] =
    Checks.nonBlank("reporting id", value).map(new ReportingId(_))

  given Codec[ReportingId] = DocumentCodecs.validated(of, _.value)

/** An inventory attribute reporting groups or filters on, such as
  * `response`.
  */
final case class Covariate private (label: String) derives CanEqual

object Covariate:
  def of(label: String): Either[DocumentError, Covariate] =
    Checks.nonBlank("covariate", label).map(new Covariate(_))

  given Codec[Covariate] = DocumentCodecs.validated(of, _.label)

/** The attribute values a filter keeps: at least one, sorted, distinct. */
final case class ValueSet private (values: Vector[String]) derives CanEqual

object ValueSet:
  def of(attribute: Covariate, values: Vector[String]): Either[DocumentError, ValueSet] =
    Either.cond(
      values.nonEmpty,
      new ValueSet(values.distinct.sorted),
      DocumentError.EmptyFilter(attribute.label)
    )

/** A share in [0, 1]. */
final case class Share private (value: Double) derives CanEqual

object Share:
  def of(value: Double): Either[DocumentError, Share] =
    Checks.finite("share", value).flatMap(Checks.within("share", _, 0.0, 1.0)).map(new Share(_))

  given Codec[Share] = DocumentCodecs.validated(of, _.value)

/** Which query trials a report keeps. */
enum ReportingFilter derives CanEqual:
  /** Keep queries whose `attribute` is one of `values`. */
  case Keep(attribute: Covariate, values: ValueSet)

  /** Keep queries with at most `share` of their fixation duration outside
    * the analysis window (DESIGN_SPEC section 9).
    */
  case OutsideWindowAtMost(share: Share)

  /** The canonical order of a spec's filters: `Keep` by attribute and
    * values, then `OutsideWindowAtMost` by share.
    */
  def sortKey: (Int, String, Double) = this match
    case Keep(attribute, values) =>
      (0, (attribute.label +: values.values).mkString("\u0000"), 0.0)
    case OutsideWindowAtMost(share) => (1, "", share.value)

object ReportingFilter:
  import io.circe.syntax.*

  given Encoder.AsObject[ReportingFilter] = Encoder.AsObject.instance {
    case Keep(attribute, values) =>
      io.circe.JsonObject(
        "Keep" -> io.circe.Json.obj(
          "attribute" -> attribute.asJson,
          "values"    -> values.values.asJson
        )
      )
    case OutsideWindowAtMost(share) =>
      io.circe.JsonObject("OutsideWindowAtMost" -> io.circe.Json.obj("share" -> share.asJson))
  }

  given Decoder[ReportingFilter] = Decoder.instance { c =>
    val keep = c.downField("Keep")
    if keep.succeeded then
      for
        attribute <- keep.get[Covariate]("attribute")
        values    <- keep.get[Vector[String]]("values")
        set       <- ValueSet
          .of(attribute, values)
          .left
          .map(e => io.circe.DecodingFailure(e.message, keep.history))
      yield Keep(attribute, set)
    else c.downField("OutsideWindowAtMost").get[Share]("share").map(OutsideWindowAtMost(_))
  }

/** The fewest contributing queries a participant needs in a group to count
  * in it. Turning it on changes n and the means.
  */
final case class MinimumPerGroup private (queries: Int) derives CanEqual

object MinimumPerGroup:
  def of(queries: Int): Either[DocumentError, MinimumPerGroup] =
    Checks.positive("minimum queries per group", queries).map(new MinimumPerGroup(_))

  given Codec[MinimumPerGroup] = DocumentCodecs.validated(of, _.queries)

/** How a report weighs query trials into its means. */
enum ReportingWeight derives CanEqual, Codec.AsObject:
  /** Participant means, each participant weighted equally (the MVP rule). */
  case ParticipantMeans

  /** Every query trial weighted equally. */
  case PooledQueries

/** An explicitly ordered within-participant subtraction between two group
  * levels. Both operands are nonblank and distinct; the run's inventory
  * validates that they are levels of the grouping covariate when reporting.
  */
final case class ReportingContrast private (minuend: String, subtrahend: String)
    derives CanEqual

object ReportingContrast:
  def of(minuend: String, subtrahend: String): Either[DocumentError, ReportingContrast] =
    for
      _ <- Checks.nonBlank("contrast minuend", minuend)
      _ <- Checks.nonBlank("contrast subtrahend", subtrahend)
      _ <- Either.cond(
        minuend != subtrahend,
        (),
        DocumentError.ContrastOperands(minuend, subtrahend)
      )
    yield new ReportingContrast(minuend, subtrahend)

  given Encoder.AsObject[ReportingContrast] =
    Encoder.forProduct2("minuend", "subtrahend")(c => (c.minuend, c.subtrahend))
  given Decoder[ReportingContrast] =
    Decoder.forProduct2("minuend", "subtrahend")(of).emap(_.left.map(_.message))

/** The studio mirror of UI-C's `ReportSpec`: how results are grouped,
  * filtered, weighted and explicitly contrasted, with no rerun.
  * `minimumPerGroup` is off (`None`) by default. `filters` is a set: kept
  * deduplicated and in [[ReportingFilter.sortKey]] order, so the same filters
  * in any order are the same spec and hash alike. A grouping without
  * `contrast` has no level subtraction; direction is never inferred.
  */
final case class ReportingSpec private (
    id: ReportingId,
    name: String,
    groupBy: Option[Covariate],
    filters: Vector[ReportingFilter],
    minimumPerGroup: Option[MinimumPerGroup],
    weighting: ReportingWeight,
    contrast: Option[ReportingContrast]
) derives CanEqual

object ReportingSpec:
  def of(
      id: ReportingId,
      name: String,
      groupBy: Option[Covariate],
      filters: Vector[ReportingFilter],
      minimumPerGroup: Option[MinimumPerGroup],
      weighting: ReportingWeight,
      contrast: Option[ReportingContrast] = None
  ): Either[DocumentError, ReportingSpec] =
    Checks
      .nonBlank("reporting name", name)
      .flatMap { checkedName =>
        Either.cond(
          groupBy.nonEmpty || contrast.isEmpty,
          checkedName,
          DocumentError.ContrastWithoutGrouping(
            id.value,
            contrast.map(_.minuend),
            contrast.map(_.subtrahend)
          )
        )
      }
      .map(
        new ReportingSpec(
          id,
          _,
          groupBy,
          filters.distinct.sortBy(_.sortKey),
          minimumPerGroup,
          weighting,
          contrast
        )
      )

  /** A spec with the defaults: no filter, no minimum, participant means. */
  def grouped(
      id: ReportingId,
      name: String,
      groupBy: Option[Covariate]
  ): Either[DocumentError, ReportingSpec] =
    of(id, name, groupBy, Vector.empty, None, ReportingWeight.ParticipantMeans)

  /** An absent contrast is omitted, preserving the bytes and identity of
    * older specs. Grouping alone never declares subtraction direction.
    */
  given Encoder.AsObject[ReportingSpec] = Encoder.AsObject.instance { s =>
    val fields = io.circe.JsonObject(
      "id"              -> s.id.asJson,
      "name"            -> s.name.asJson,
      "groupBy"         -> s.groupBy.asJson,
      "filters"         -> s.filters.asJson,
      "minimumPerGroup" -> s.minimumPerGroup.asJson,
      "weighting"       -> s.weighting.asJson
    )
    s.contrast.fold(fields)(c => fields.add("contrast", c.asJson))
  }
  given Decoder[ReportingSpec] = Decoder.instance { c =>
    for
      id        <- c.get[ReportingId]("id")
      name      <- c.get[String]("name")
      groupBy   <- c.get[Option[Covariate]]("groupBy")
      filters   <- c.get[Vector[ReportingFilter]]("filters")
      minimum   <- c.get[Option[MinimumPerGroup]]("minimumPerGroup")
      weighting <- c.get[ReportingWeight]("weighting")
      contrast  <- c.get[Option[ReportingContrast]]("contrast")
      spec      <- of(id, name, groupBy, filters, minimum, weighting, contrast).left.map(e =>
        io.circe.DecodingFailure(e.message, c.history)
      )
    yield spec
  }

  /** The CR3 digest of `spec`: its identity in an export's provenance. A
    * spec has no revision and is edited in place, so the digest, not the
    * id, says which version of it a figure was exported with.
    */
  def digest(spec: ReportingSpec): Either[CodecError, CanonicalDigest[ReportingSpec]] =
    specCodec.flatMap(_.digest(spec))

  /** Version 2 records explicit contrast operands. Earlier specs lift
    * unchanged, with no contrast; a version-1 envelope cannot smuggle one.
    */
  val ladder: Either[CodecError, SchemaLadder[ReportingSpec]] =
    StudioSchemaIds.forCodec.map { ids =>
      def write(s: ReportingSpec): Either[CodecError, io.circe.Json] =
        Right(CanonicalJson(s.asJson))
      def read(json: io.circe.Json): Either[CodecError, ReportingSpec] =
        json.as[ReportingSpec].left.map(f => CodecError.Field("reporting", json, f.getMessage))
      def before(s: ReportingSpec): Either[CodecError, ReportingSpec] =
        Either.cond(s.contrast.isEmpty, s, contrastVersionError)
      SchemaLadder
        .of[ReportingSpec]("studio reporting spec", ids.reporting)(s =>
          before(s).flatMap(write)
        )(json => read(json).flatMap(before))
        .next(_.contrast.isEmpty, identity)(write)(read)
    }

  private val specCodec: Either[CodecError, VersionedCodec[ReportingSpec]] = ladder.map(_.codec)

  private[document] val contrastVersionError: CodecError =
    CodecError.Unsupported("studio reporting spec", "explicit contrast operands need version 2")

// ---------------------------------------------------------------------------
// Figures
// ---------------------------------------------------------------------------

/** A figure's number, displayed `Figure 1`. */
final case class FigureId private (number: Int) derives CanEqual:
  def label: String = s"Figure $number"

object FigureId:
  def of(number: Int): Either[DocumentError, FigureId] =
    Checks.positive("figure number", number).map(new FigureId(_))

  given Codec[FigureId] = DocumentCodecs.validated(of, _.number)

/** A panel letter, `A` to `Z`. */
final case class PanelLetter private (value: String) derives CanEqual

object PanelLetter:
  def of(value: String): Either[DocumentError, PanelLetter] =
    Either.cond(
      value.length == 1 && value.head >= 'A' && value.head <= 'Z',
      new PanelLetter(value),
      DocumentError.BadPanelLetter(value)
    )

  given Codec[PanelLetter] = DocumentCodecs.validated(of, _.value)

/** The scale a panel shows: none (a gaze plot), one, or every scale of the
  * run (a scale profile).
  */
enum PanelScale derives CanEqual, Codec.AsObject:
  case Unscaled
  case At(sigma: Sigma)
  case AllScales

/** The trials a panel shows. */
enum PanelSelection derives CanEqual, Codec.AsObject:
  /** One trial, such as P17 enc_03. */
  case Trial(key: TrialKey)

  /** One query with its matched reference and highest-scoring control. */
  case QueryWithReferences(query: TrialKey)

  /** Every query the figure's reporting spec keeps. */
  case AllQueries

/** One panel: a panel chooses only its scale and trial selection
  * (DESIGN_SPEC section 12, figure binding).
  */
final case class PanelSpec(
    letter: PanelLetter,
    title: String,
    scale: PanelScale,
    selection: PanelSelection
) derives CanEqual,
      Codec.AsObject

/** A figure binds one run and one reporting spec; it never follows the
  * latest result. Panels have distinct letters and there is at least one.
  */
final case class FigureSpec private (
    id: FigureId,
    run: RunId,
    reporting: ReportingId,
    panels: Vector[PanelSpec]
) derives CanEqual

object FigureSpec:
  def of(
      id: FigureId,
      run: RunId,
      reporting: ReportingId,
      panels: Vector[PanelSpec]
  ): Either[DocumentError, FigureSpec] =
    val repeated = panels.map(_.letter).diff(panels.map(_.letter).distinct).distinct
    if panels.isEmpty then Left(DocumentError.NoPanels(id))
    else if repeated.nonEmpty then
      Left(DocumentError.DuplicatePanels(id, repeated.map(_.value)))
    else Right(new FigureSpec(id, run, reporting, panels))

  given Encoder.AsObject[FigureSpec] =
    Encoder.forProduct4("id", "run", "reporting", "panels")(f =>
      (f.id, f.run, f.reporting, f.panels)
    )
  given Decoder[FigureSpec] =
    Decoder.forProduct4("id", "run", "reporting", "panels")(of).emap(_.left.map(_.message))

  extension (figure: FigureSpec)
    /** The scales its panels name, for checking against the run. */
    def namedScales: Vector[(PanelLetter, Sigma)] = figure.panels.collect {
      case PanelSpec(letter, _, PanelScale.At(sigma), _) => letter -> sigma
    }
