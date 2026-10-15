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

package eyes4s.results

import eyes4s.aoi.*
import eyes4s.kernel.*

/** Fixation AOI results in the shared table layer. Callers supply their trial key as
  * JSON; no recording, samples or study grouping are fabricated. Exact Int64 times
  * and counts, null validity and absence reasons survive the shared CSV/Arrow writers.
  * Context binds the spatial policy, exact frame, source evidence and input content.
  */
object FixationAoiTables:
  import ResultCell.{Text as T, Integer as I, Number as N, Missing as M}

  private def text(name: String, meaning: String) =
    ResultColumn(name, ResultColumnType.Utf8, false, "text", meaning)
  private def count(name: String, meaning: String) =
    ResultColumn(name, ResultColumnType.Int64, false, "count", meaning)
  private def time(name: String, meaning: String, nullable: Boolean = false) =
    ResultColumn(name, ResultColumnType.Int64, nullable, "microseconds", meaning)
  private val keyColumn = ResultColumn(
    "key_json",
    ResultColumnType.JsonUtf8,
    false,
    "text",
    "caller-supplied trial key"
  )
  private val metricColumns = Vector(
    keyColumn,
    text("aoi_id", "nominal area identity"),
    text("aoi_label", "area label"),
    time("dwell_us", "sum of contained fixation durations; gaps excluded"),
    time(
      "analysable_fixation_us",
      "on-surface fixation time including background; proportion denominator"
    ),
    ResultColumn(
      "dwell_proportion",
      ResultColumnType.Float64,
      true,
      "proportion",
      "dwell divided by analysable_fixation_us"
    ),
    text("dwell_proportion_status", "present or no-analysable-fixation-time"),
    time(
      "first_entry_latency_us",
      "first contained onset relative to first fixation onset",
      nullable = true
    ),
    text("first_entry_status", "present or never-entered"),
    count("fixation_count", "contained fixation count"),
    count("entry_count", "one entry per visit, including initially contained fixation"),
    count(
      "visit_count",
      "maximal consecutive contained fixation runs; gaps do not break visits"
    )
  )
  private val transitionColumns = Vector(
    keyColumn,
    text("from_aoi", "transition origin area identity"),
    text("to_aoi", "transition destination area identity"),
    count(
      "transition_count",
      "ordered transitions between adjacent fixations; gaps do not break the chain"
    )
  )

  /** Metrics and transitions, sharing one computed context and input digest. */
  def tables[U <: Unit2D: UnitLabel](
      assignment: FixationAoiAssignment[U],
      key: TableJson
  ): Either[ResultTableError, Vector[ResultTable]] =
    val measured = assignment.measure
    val metadata = context(assignment)
    for
      metrics <- ResultTable.of(
        ResultFamily.FixationAoiMetrics,
        metricColumns,
        measured.areas.map { m =>
          Vector(
            T(key.canonical),
            T(m.id.value),
            T(m.label),
            I(m.dwell.toMicros),
            I(assignment.report.analysableTime.toMicros),
            m.dwellProportion.fold[ResultCell](M)(N.apply),
            T(if m.dwellProportion.isDefined then "present" else "no-analysable-fixation-time"),
            m.firstEntryLatency.fold[ResultCell](M)(s => I(s.toMicros)),
            T(if m.firstEntryLatency.isDefined then "present" else "never-entered"),
            I(m.fixationCount.toLong),
            I(m.entryCount.toLong),
            I(m.visitCount.toLong)
          )
        },
        metadata
      )
      transitions <- ResultTable.of(
        ResultFamily.FixationAoiTransitions,
        transitionColumns,
        measured.transitions.map(t =>
          Vector(T(key.canonical), T(t.from.value), T(t.to.value), I(t.count.toLong))
        ),
        metadata
      )
    yield Vector(metrics, transitions)

  private def decimal(value: Double): TableJson = TableJson.text(DecimalText.render(value))
  private def point[U <: Unit2D](p: Pt[U]): TableJson =
    TableJson.arr(decimal(p.x), decimal(p.y))
  private def region[U <: Unit2D](r: Region[U]): TableJson = r match
    case rect: Region.Rect[U] =>
      TableJson.obj(
        "kind" -> TableJson.text("rect"),
        "lo"   -> point(rect.lo),
        "hi"   -> point(rect.hi)
      )
    case ellipse: Region.Ellipse[U] =>
      TableJson.obj(
        "kind"   -> TableJson.text("ellipse"),
        "centre" -> point(ellipse.centre),
        "rx"     -> decimal(ellipse.rx),
        "ry"     -> decimal(ellipse.ry)
      )
    case polygon: Region.Polygon[U] =>
      TableJson.obj(
        "kind"     -> TableJson.text("polygon"),
        "vertices" -> TableJson.Arr(polygon.vertices.map(point))
      )
    case union: Region.Union[U] =>
      TableJson.obj(
        "kind" -> TableJson.text("union"),
        "a"    -> region(union.a),
        "b"    -> region(union.b)
      )
    case intersect: Region.Intersect[U] =>
      TableJson.obj(
        "kind" -> TableJson.text("intersect"),
        "a"    -> region(intersect.a),
        "b"    -> region(intersect.b)
      )
    case complement: Region.Complement[U] =>
      TableJson.obj("kind" -> TableJson.text("complement"), "a" -> region(complement.a))
    case _: Region.Empty[U]      => TableJson.obj("kind" -> TableJson.text("empty"))
    case _: Region.Everything[U] => TableJson.obj("kind" -> TableJson.text("everything"))

  private def policy(p: MembershipPolicy): TableJson = p match
    case MembershipPolicy.SmallestContaining(r) =>
      TableJson.obj(
        "kind" -> TableJson.text("smallest-containing"),
        "nx"   -> TableJson.integer(r.nx.toLong),
        "ny"   -> TableJson.integer(r.ny.toLong)
      )
    case MembershipPolicy.Multiple => TableJson.obj("kind" -> TableJson.text("multiple"))
    case MembershipPolicy.ExclusiveByPriority =>
      TableJson.obj("kind" -> TableJson.text("exclusive-by-priority"))
    case MembershipPolicy.RejectOverlap =>
      TableJson.obj("kind" -> TableJson.text("reject-overlap"))

  private def context[U <: Unit2D: UnitLabel](a: FixationAoiAssignment[U]): TableJson =
    val f     = a.scanpath.frame
    val frame = TableJson.obj(
      "id"    -> TableJson.text(f.id.name),
      "unit"  -> TableJson.text(summon[UnitLabel[U]].symbol),
      "xMin"  -> decimal(f.spec.xMin),
      "yMin"  -> decimal(f.spec.yMin),
      "xMax"  -> decimal(f.spec.xMax),
      "yMax"  -> decimal(f.spec.yMax),
      "yAxis" -> TableJson.text(f.yAxis.toString)
    )
    val source = (a.scanpath.source, a.scanpath.sourceRecording, a.scanpath.sampleSupport) match
      case (Some(ref), Some(recording), Some(support)) =>
        TableJson.obj(
          "ref"            -> TableJson.text(ref.value),
          "recording_hash" -> TableJson.text(recording.contentHash.render),
          "support"        -> TableJson.Arr(
            support.map(r =>
              TableJson.arr(TableJson.integer(r.from.toLong), TableJson.integer(r.until.toLong))
            )
          )
        )
      case _ => TableJson.Null
    val clock         = TableJson.text(a.scanpath.clock.name)
    val spatialPolicy = policy(a.policy)
    val inputs        = TableJson.obj(
      "frame"     -> frame,
      "clock"     -> clock,
      "policy"    -> spatialPolicy,
      "source"    -> source,
      "fixations" -> TableJson.Arr(
        a.scanpath.fixations.toVector.map(fix =>
          TableJson.obj(
            "onset_us"     -> TableJson.integer(fix.span.onset.toMicros),
            "offset_us"    -> TableJson.integer(fix.span.offset.toMicros),
            "centre"       -> point(fix.centre),
            "sample_count" -> TableJson.integer(fix.sampleCount.toLong)
          )
        )
      ),
      "areas" -> TableJson.Arr(
        a.aoiSet.areas.map(area =>
          TableJson.obj(
            "id"         -> TableJson.text(area.id.value),
            "label"      -> TableJson.text(area.label),
            "region"     -> region(area.region),
            "attributes" -> TableJson.Obj(
              area.attributes.toVector.sortBy(_._1).map((k, v) => k -> TableJson.text(v))
            )
          )
        )
      )
    )
    TableJson.obj(
      "analysis"         -> TableJson.text("fixation-aoi-visits/1"),
      "frame"            -> frame,
      "clock"            -> clock,
      "policy"           -> spatialPolicy,
      "source"           -> source,
      "input_sha256"     -> TableJson.text(TableDigest.ofUtf8(inputs.canonical).hex),
      "visit_convention" -> TableJson.text("consecutive-fixations-ignore-gaps"),
      "entry_convention" -> TableJson.text("first-fixation-of-each-visit-including-initial"),
      "denominator"      -> TableJson.text("on-surface-fixation-duration-including-background"),
      "report"           -> TableJson.obj(
        "aoi_union_us"      -> TableJson.integer(a.report.aoiUnionTime.toMicros),
        "background_us"     -> TableJson.integer(a.report.backgroundTime.toMicros),
        "off_surface_us"    -> TableJson.integer(a.report.offSurfaceTime.toMicros),
        "inter_fixation_us" -> TableJson.integer(a.report.interFixationTime.toMicros),
        "duplicated_aoi_us" -> TableJson.integer(a.report.duplicatedAoiTime.toMicros)
      )
    )
