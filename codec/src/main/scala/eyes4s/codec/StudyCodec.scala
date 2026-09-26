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

package eyes4s.codec

import cats.syntax.all.*
import eyes4s.compare.*
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.plan.*
import eyes4s.surface.*
import io.circe.Json

/** Built-in identities introduced with windowed, angular and occurrence-aware plans. */
object StudyCodecDefinitions:
  /** A study plan that records its geometry (whole frame or analysis window),
    * its declared scales and units-per-degree, and its pairing policy.
    */
  val studyV2: DefinitionId = DefinitionId.builtIn("eyes4s.study", 2)

  /** A version-2 study plan that also records its initial-fixation policy. */
  val studyV3: DefinitionId = DefinitionId.builtIn("eyes4s.study", 3)

/** Codecs for the concrete geometry needed by a fixation study. */
object StudyCodecs:
  def cosine[U <: Unit2D](using
      UnitLabel[U]
  ): StudyCodec[StudyKey, U, Unit, Similarity, SignedDifference] =
    similarity[U](ComparisonMethods.cosine)

  /** The ordinary participant/stimulus/phase route of a registered map
    * method, matching `StudyPlan.similarity`; the plan records the method's
    * built-in identity.
    */
  def similarity[U <: Unit2D](method: ComparisonMethod)(using
      UnitLabel[U]
  ): StudyCodec[StudyKey, U, Unit, Similarity, SignedDifference] =
    new StudyCodec(
      DefinitionId.study,
      StudyKey.layout(DefinitionId.studyLayout),
      key(DefinitionId.studyKey),
      method.study[U],
      VersionedCodec.unit(DefinitionId.unit)
    )

  /** The trial-keyed cosine study: trials identified by participant, phase,
    * trial and occurrence, matched on their item.
    */
  def trialCosine[U <: Unit2D](using
      UnitLabel[U]
  ): StudyCodec[TrialKey, U, Unit, Similarity, SignedDifference] =
    new StudyCodec(
      DefinitionId.study,
      TrialKey.layout(TrialKeyDefinitions.trialLayout),
      trialKey(TrialKeyDefinitions.trialKey),
      ComparisonMethods.cosine.study[U],
      VersionedCodec.unit(DefinitionId.unit)
    )

  /** A [[TrialKey]] with its item beside its identity. */
  def trialKey(schema: DefinitionId): VersionedCodec[TrialKey] =
    VersionedCodec.of[TrialKey](schema)(k =>
      Json.obj(
        "participant" -> Json.fromString(k.participant),
        "phase"       -> Json.fromString(k.phase),
        "trial"       -> Json.fromString(k.trial),
        "occurrence"  -> Json.fromInt(k.occurrence.value),
        "item"        -> Json.fromString(k.item)
      )
    ) { json =>
      for
        participant <- Wire.field[String](json, "participant")
        phase       <- Wire.field[String](json, "phase")
        trial       <- Wire.field[String](json, "trial")
        raw         <- Wire.field[Int](json, "occurrence")
        occurrence  <- TrialOccurrence.of(raw).left.map(CodecError.Definition.apply)
        item        <- Wire.field[String](json, "item")
        key         <- TrialKey
          .of(participant, phase, trial, occurrence, item)
          .left
          .map(CodecError.Definition.apply)
      yield key
    }

  def key(schema: DefinitionId): VersionedCodec[StudyKey] =
    VersionedCodec.of[StudyKey](schema)(k =>
      Json.obj(
        "participant" -> Json.fromString(k.participant),
        "stimulus"    -> Json.fromString(k.stimulus),
        "phase"       -> Json.fromString(k.phase)
      )
    ) { json =>
      for
        participant <- Wire.field[String](json, "participant")
        stimulus    <- Wire.field[String](json, "stimulus")
        phase       <- Wire.field[String](json, "phase")
      yield StudyKey(participant, stimulus, phase)
    }

  private[codec] def frame[U <: Unit2D: UnitLabel](f: Frame[U]): Json = DomainWire.frame(f)

  private[codec] def readFrame[U <: Unit2D: UnitLabel](
      json: Json
  ): Either[CodecError, Frame[U]] =
    DomainWire.readFrame[U](json)

/** Persistence registration captures the typed key and parameter codecs with one method.
  * Plan has no circe dependency. Runtime lookup selects this already typed closure;
  * no Any-valued parameter registry or cast is involved (bead bd-01KYDZ7GGMYKTXYWNRR40Q8X9M).
  */
final class StudyCodec[K, U <: Unit2D, P, S, D](
    val schema: DefinitionId,
    val layout: StudyLayout[K],
    val keys: VersionedCodec[K],
    val method: StudyMethod[P, U, S, D],
    val parameters: VersionedCodec[P]
)(using unit: UnitLabel[U]):
  /** The plan schema that records geometry, declared scales and pairing. */
  val schemaV2: DefinitionId = DefinitionId.builtIn(schema.name, schema.version + 1)

  /** The plan schema that also records the initial-fixation policy. */
  val schemaV3: DefinitionId = DefinitionId.builtIn(schema.name, schema.version + 2)

  /** The three plan versions. A version-1 plan maps the whole frame with
    * scales in frame units, averages every matched reference and keeps every
    * fixation; a version-2 plan keeps every fixation. A plan is written under
    * the earliest version that expresses it, so version-1 and version-2
    * plans re-encode to their own bytes. The upcasts state what each earlier
    * version left implicit (`StudyCodec.upcastV1`, `StudyCodec.upcastV2`).
    */
  val ladder: SchemaLadder[StudyPlan[K, U, P, S, D]] =
    SchemaLadder
      .of[StudyPlan[K, U, P, S, D]]("study plan", schema)(write)(read(_, version = 1))
      .next(_.isVersion1, StudyCodec.upcastV1)(writeVersion2)(read(_, version = 2))
      .next(_.keepsAllFixations, StudyCodec.upcastV2)(plan =>
        writeVersion2(plan).map(
          Wire.append(
            _,
            Json.obj("initialFixations" -> StudyWire.initialFixations(plan.initialFixations))
          )
        )
      )(read(_, version = 3))

  /** The version-2 payload: `estimates` replaced by the declared `scales`. */
  private def writeVersion2(plan: StudyPlan[K, U, P, S, D]): Either[CodecError, Json] =
    for
      base  <- write(plan)
      extra <- writeV2(plan)
    yield Wire.append(base.mapObject(_.remove("estimates")), extra)

  val codec: VersionedCodec[StudyPlan[K, U, P, S, D]] = ladder.codec

  /** The version-2 members; `estimates` is replaced by the declared `scales`. */
  private def writeV2(plan: StudyPlan[K, U, P, S, D]): Either[CodecError, Json] =
    val geometry = plan.geometry match
      case StudyGeometry.WholeFrame(_) => Json.obj("kind" -> Json.fromString("wholeFrame"))
      case StudyGeometry.Windowed(window, _, policy) =>
        Json.obj(
          "kind"      -> Json.fromString("windowed"),
          "admission" -> StudyCodecs.frame(window.parent),
          "window"    -> Json.obj(
            "id"   -> Json.fromString(window.frame.id.name),
            "xMin" -> Json.fromDoubleOrNull(window.region.xMin),
            "yMin" -> Json.fromDoubleOrNull(window.region.yMin),
            "xMax" -> Json.fromDoubleOrNull(window.region.xMax),
            "yMax" -> Json.fromDoubleOrNull(window.region.yMax)
          ),
          "offWindow" -> Json.fromString(StudyWire.offWindow(policy))
        )
    Right(
      Json.obj(
        "pairing"      -> StudyWire.pairing(plan.pairing),
        "geometry"     -> geometry,
        "scales"       -> Json.arr(plan.scales.map(StudyWire.scale[U])*),
        "angularScale" -> plan.angularScale.fold(Json.Null)(s =>
          Json.obj(
            "frame"          -> Json.fromString(s.frame.id.name),
            "unitsPerDegree" -> Json.fromDoubleOrNull(s.unitsPerDegree)
          )
        )
      )
    )

  private def write(plan: StudyPlan[K, U, P, S, D]): Either[CodecError, Json] = for
    _ <- Either.cond(
      plan.layout.id == layout.id,
      (),
      CodecError.Schema(layout.id, plan.layout.id)
    )
    _ <- Either.cond(
      plan.method.id == method.id,
      (),
      CodecError.Schema(method.id, plan.method.id)
    )
    encodedParameters <- parameters.encode(plan.parameters)
  yield Json.obj(
    "layout"         -> Wire.id(plan.layout.id),
    "keySchema"      -> Wire.id(keys.schema),
    "method"         -> Wire.id(plan.method.id),
    "input"          -> Json.fromString(plan.input.digest),
    "frame"          -> StudyCodecs.frame(plan.grid.frame),
    "gridId"         -> Json.fromString(plan.grid.id.name),
    "nx"             -> Json.fromInt(plan.grid.nx),
    "ny"             -> Json.fromInt(plan.grid.ny),
    "focalPhase"     -> Json.fromString(plan.focalPhase),
    "referencePhase" -> Json.fromString(plan.referencePhase),
    "weight"         -> Json.fromString(plan.weight.toString),
    "policy"         -> StudyWire.policy(plan.policy),
    "estimates"      -> Json.arr(plan.estimates.map(StudyWire.estimate[U])*),
    "parameters"     -> encodedParameters
  )

  private def requireId(
      json: Json,
      field: String,
      expected: DefinitionId
  ): Either[CodecError, Unit] =
    Wire
      .definition(json, field)
      .flatMap(found => Either.cond(found == expected, (), CodecError.Schema(expected, found)))

  private def read(json: Json, version: Int): Either[CodecError, StudyPlan[K, U, P, S, D]] = for
    _      <- requireId(json, "layout", layout.id)
    _      <- requireId(json, "keySchema", keys.schema)
    _      <- requireId(json, "method", method.id)
    digest <- Wire.field[String](json, "input")
    input  <- ArtifactRef.parse[StudyInput[K, U]](digest).left.map(CodecError.Definition.apply)
    frameJson <- Wire.field[Json](json, "frame")
    frame     <- StudyCodecs.readFrame[U](frameJson)
    gridName  <- Wire.field[String](json, "gridId")
    nx        <- Wire.field[Int](json, "nx")
    ny        <- Wire.field[Int](json, "ny")
    grid      <- Grid
      .of(GridId(gridName), frame, nx, ny)
      .left
      .map(e => CodecError.Field("grid", json, e.message))
    focal      <- Wire.field[String](json, "focalPhase")
    reference  <- Wire.field[String](json, "referencePhase")
    weightName <- Wire.field[String](json, "weight")
    weight     <- Weight.values
      .find(_.toString == weightName)
      .toRight(CodecError.Field("weight", json, s"unknown weight $weightName"))
    policy        <- Wire.field[Json](json, "policy").flatMap(StudyWire.readPolicy)
    parameterJson <- Wire.field[Json](json, "parameters")
    params        <- parameters.decode(parameterJson)
    plan          <-
      if version == 1 then
        for
          estimateJson <- Wire.field[Vector[Json]](json, "estimates")
          estimates    <- estimateJson.traverse(StudyWire.readEstimate[U])
          plan         <- StudyPlan
            .of(
              input,
              layout,
              grid,
              focal,
              reference,
              weight,
              estimates,
              policy,
              method,
              params
            )
            .left
            .map(CodecError.Definition.apply)
        yield plan
      else
        for
          geometryJson <- Wire.field[Json](json, "geometry")
          geometry     <- readGeometry(geometryJson, grid)
          scaleJson    <- Wire.field[Vector[Json]](json, "scales")
          scales       <- scaleJson.zipWithIndex.traverse { case (entry, index) =>
            StudyWire.readScale[U](entry).left.map(Wire.at(s"scales[$index]"))
          }
          angularJson <- Wire.field[Option[Json]](json, "angularScale")
          angular     <- angularJson.filterNot(_.isNull).traverse(readAngular(_, geometry))
          pairing     <- Wire.field[Json](json, "pairing").flatMap(StudyWire.readPairing)
          initial     <-
            if version < 3 then Right(InitialFixationPolicy.keepAll[U])
            else
              Wire
                .field[Json](json, "initialFixations")
                .flatMap(StudyWire.readInitialFixations[U])
          plan <- StudyPlan
            .configure(
              input,
              layout,
              geometry,
              focal,
              reference,
              weight,
              scales,
              angular,
              policy,
              method,
              params,
              pairing,
              initial
            )
            .left
            .map(CodecError.Definition.apply)
        yield plan
  yield plan

  private def readGeometry(json: Json, grid: Grid[U]): Either[CodecError, StudyGeometry[U]] =
    Wire.field[String](json, "kind").flatMap {
      case "wholeFrame" => Right(StudyGeometry.WholeFrame(grid))
      case "windowed"   =>
        for
          admissionJson <- Wire.field[Json](json, "admission")
          admission     <- StudyCodecs.readFrame[U](admissionJson)
          window        <- Wire.field[Json](json, "window")
          id            <- Wire.field[String](window, "id")
          x0            <- DomainWire.finite(window, "xMin")
          y0            <- DomainWire.finite(window, "yMin")
          x1            <- DomainWire.finite(window, "xMax")
          y1            <- DomainWire.finite(window, "yMax")
          region        <- Bounds
            .of[U](x0, y0, x1, y1)
            .left
            .map(e => CodecError.Field("window", window, e.message))
          subframe <- Subframe
            .of(admission, FrameId(id), region)
            .left
            .map(e => CodecError.Field("window", window, e.message))
          name      <- Wire.field[String](json, "offWindow")
          offWindow <- StudyWire
            .readOffWindow(name)
            .toRight(CodecError.Field("offWindow", json, s"unknown off-window policy $name"))
          geometry <- StudyGeometry
            .windowed(subframe, grid, offWindow)
            .left
            .map(e => CodecError.Field("window", window, e.message))
        yield geometry
      case other => Left(CodecError.Field("geometry", json, s"unknown geometry $other"))
    }

  private def readAngular(
      json: Json,
      geometry: StudyGeometry[U]
  ): Either[CodecError, LinearAngularScale[U]] =
    for
      frame <- Wire.field[String](json, "frame")
      _     <- Either.cond(
        frame == geometry.admission.id.name,
        (),
        CodecError.Field(
          "frame",
          json,
          s"units per degree are declared on frame $frame, but the plan admits ${geometry.admission.id.name}"
        )
      )
      value <- DomainWire.finite(json, "unitsPerDegree")
      scale <- LinearAngularScale
        .of(geometry.admission, value)
        .left
        .map(e => CodecError.Field("unitsPerDegree", json, e.message))
    yield scale

  /** The result archive for this plan family, with explicit score and difference codecs. */
  def results(
      scores: VersionedCodec[S],
      differences: VersionedCodec[D]
  ): StudyResultCodec[K, U, P, S, D] =
    new StudyResultCodec(DefinitionId.studyResult, layout, keys, method, scores, differences)

  def registration: StudyRegistration[K, U] =
    new StudyRegistration[K, U]:
      val id                                                        = method.id
      def decode(json: Json): Either[CodecError, LoadedStudy[K, U]] =
        codec.decode(json).map { value =>
          new LoadedStudy[K, U]:
            type Parameters = P
            type Score      = S
            type Difference = D
            val plan   = value
            def encode = codec.encode(value)
        }

private[codec] object StudyCodec:
  /** Lift a version-1 plan payload to version 2: its `estimates` become
    * native `scales`, and the geometry, pairing and angular scale it left
    * implicit are stated (the whole frame, `StudyPairing.version1`, none).
    */
  def upcastV1(payload: Json): Json =
    val estimates = payload.hcursor.downField("estimates").focus
    val scales    = estimates
      .flatMap(_.asArray)
      .fold(estimates.getOrElse(Json.Null))(entries =>
        Json.arr(
          entries.map(e => Json.obj("kind" -> Json.fromString("native"), "estimate" -> e))*
        )
      )
    Wire.append(
      payload.mapObject(_.remove("estimates")),
      Json.obj(
        "pairing"      -> StudyWire.pairing(StudyPairing.version1),
        "geometry"     -> Json.obj("kind" -> Json.fromString("wholeFrame")),
        "scales"       -> scales,
        "angularScale" -> Json.Null
      )
    )

  /** Lift a version-2 plan payload to version 3: the initial-fixation policy
    * version 2 left implicit is stated, keeping every fixation.
    */
  def upcastV2(payload: Json): Json =
    Wire.append(
      payload,
      Json.obj(
        "initialFixations" -> StudyWire.initialFixations(
          InitialFixationPolicy.keepAll[Unit2D.Px]
        )
      )
    )

/** A study plan resolved by method identity at run time. Its parameter,
  * score and difference types are abstract but fixed, so `plan` is the typed
  * plan itself: preflight, prepare, execute and inspect it exactly as the
  * plan the application built, with no encode and re-decode.
  */
sealed trait LoadedStudy[K, U <: Unit2D]:
  type Parameters
  type Score
  type Difference

  /** The typed plan, as decoded. */
  def plan: StudyPlan[K, U, Parameters, Score, Difference]
  def encode: Either[CodecError, Json]

  def description: Vector[(String, Vector[Provenance.Param])]           = plan.description
  def prerequisites(input: Option[StudyInput[K, U]]): Vector[PlanError] =
    plan.prerequisites(input)

  /** The plan's typed availability report; see `StudyPlan.preflight`. */
  def preflight(
      available: Option[StudyInput[K, U]],
      budget: PairScheduleBudget = PairScheduleBudget.default
  ): StudyReport[K, U] = plan.preflight(available, budget)

  def run(input: StudyInput[K, U]): Either[PlanError, StudyResult[K, U, Score, Difference]] =
    plan.run(input)

sealed trait StudyRegistration[K, U <: Unit2D]:
  def id: DefinitionId
  def decode(json: Json): Either[CodecError, LoadedStudy[K, U]]

final class StudyRegistry[K, U <: Unit2D] private (
    val entries: Vector[StudyRegistration[K, U]]
):
  def register(entry: StudyRegistration[K, U]): Either[CodecError, StudyRegistry[K, U]] =
    if entries.exists(_.id == entry.id) then Left(CodecError.DuplicateMethod(entry.id))
    else Right(new StudyRegistry(entries :+ entry))
  def decode(json: Json): Either[CodecError, LoadedStudy[K, U]] = for
    payload    <- Wire.field[Json](json, "value")
    method     <- Wire.definition(payload, "method")
    registered <- entries.find(_.id == method).toRight(CodecError.MissingMethod(method))
    result     <- registered.decode(json)
  yield result
object StudyRegistry:
  def empty[K, U <: Unit2D]: StudyRegistry[K, U] = new StudyRegistry(Vector.empty)

/** Wire forms shared by plan and result payloads. */
private[codec] object StudyWire:
  def policy(value: FailurePolicy): Json = value match
    case FailurePolicy.RequireAll => Json.obj("kind" -> Json.fromString("requireAll"))
    case FailurePolicy.SuccessfulOnly(minimum) =>
      Json.obj(
        "kind"    -> Json.fromString("successfulOnly"),
        "minimum" -> Json.fromInt(minimum.value)
      )

  def readPolicy(json: Json): Either[CodecError, FailurePolicy] =
    Wire.field[String](json, "kind").flatMap {
      case "requireAll"     => Right(FailurePolicy.RequireAll)
      case "successfulOnly" =>
        Wire
          .field[Int](json, "minimum")
          .flatMap(n =>
            FailurePolicy
              .successfulOnly(n)
              .left
              .map(e => CodecError.Field("minimum", json, e.message))
          )
      case other => Left(CodecError.Field("policy", json, s"unknown policy $other"))
    }

  def matched(value: MatchedReferences): Json = value match
    case MatchedReferences.RequireOne     => Json.obj("kind" -> Json.fromString("requireOne"))
    case MatchedReferences.SameOccurrence =>
      Json.obj("kind" -> Json.fromString("sameOccurrence"))
    case MatchedReferences.MeanOfAll      => Json.obj("kind" -> Json.fromString("meanOfAll"))
    case MatchedReferences.Select(choice) =>
      Json.obj(
        "kind"   -> Json.fromString("select"),
        "choice" -> (choice match
          case OccurrenceChoice.First => Json.obj("kind" -> Json.fromString("first"))
          case OccurrenceChoice.Last  => Json.obj("kind" -> Json.fromString("last"))
          case OccurrenceChoice.At(n) =>
            Json.obj("kind" -> Json.fromString("at"), "occurrence" -> Json.fromInt(n.value)))
      )

  def readMatched(json: Json): Either[CodecError, MatchedReferences] =
    Wire.field[String](json, "kind").flatMap {
      case "requireOne"     => Right(MatchedReferences.RequireOne)
      case "sameOccurrence" => Right(MatchedReferences.SameOccurrence)
      case "meanOfAll"      => Right(MatchedReferences.MeanOfAll)
      case "select"         =>
        Wire.field[Json](json, "choice").flatMap { choice =>
          Wire.field[String](choice, "kind").flatMap {
            case "first" => Right(MatchedReferences.Select(OccurrenceChoice.First))
            case "last"  => Right(MatchedReferences.Select(OccurrenceChoice.Last))
            case "at"    =>
              Wire
                .field[Int](choice, "occurrence")
                .flatMap(TrialOccurrence.of(_).left.map(CodecError.Definition.apply))
                .map(n => MatchedReferences.Select(OccurrenceChoice.At(n)))
            case other =>
              Left(CodecError.Field("choice", choice, s"unknown occurrence choice $other"))
          }
        }
      case other =>
        Left(CodecError.Field("matched", json, s"unknown matched-reference rule $other"))
    }

  private val controlNames   = ControlReferences.values.toVector.map(v => v -> v.name)
  private val unmatchedNames = UnmatchedFocalPolicy.values.toVector.map(v => v -> v.name)

  def pairing(value: StudyPairing): Json = Json.obj(
    "matched"   -> matched(value.matched),
    "controls"  -> Json.fromString(controlNames.toMap.apply(value.controls)),
    "unmatched" -> Json.fromString(unmatchedNames.toMap.apply(value.unmatched))
  )

  def readPairing(json: Json): Either[CodecError, StudyPairing] = for
    m        <- Wire.field[Json](json, "matched").flatMap(readMatched)
    c        <- Wire.field[String](json, "controls")
    controls <- controlNames
      .collectFirst { case (v, `c`) => v }
      .toRight(CodecError.Field("controls", json, s"unknown control pool $c"))
    u         <- Wire.field[String](json, "unmatched")
    unmatched <- unmatchedNames
      .collectFirst { case (v, `u`) => v }
      .toRight(CodecError.Field("unmatched", json, s"unknown unmatched-focal policy $u"))
  yield StudyPairing(m, controls, unmatched)

  def initialFixations[U <: Unit2D](policy: InitialFixationPolicy[U]): Json = policy match
    case InitialFixationPolicy.KeepAll()   => Json.obj("kind" -> Json.fromString("keepAll"))
    case InitialFixationPolicy.DropFirst() => Json.obj("kind" -> Json.fromString("dropFirst"))
    case InitialFixationPolicy.DropLeadingInClosedDisc(cross, radius) =>
      Json.obj(
        "kind"  -> Json.fromString("dropLeadingInClosedDisc"),
        "cross" -> Json.obj(
          "x" -> Json.fromDoubleOrNull(cross.x),
          "y" -> Json.fromDoubleOrNull(cross.y)
        ),
        "radiusDegrees" -> Json.fromDoubleOrNull(radius)
      )

  def readInitialFixations[U <: Unit2D](
      json: Json
  ): Either[CodecError, InitialFixationPolicy[U]] =
    Wire.field[String](json, "kind").flatMap {
      case "keepAll"                 => Right(InitialFixationPolicy.keepAll[U])
      case "dropFirst"               => Right(InitialFixationPolicy.dropFirst[U])
      case "dropLeadingInClosedDisc" =>
        for
          cross  <- Wire.field[Json](json, "cross")
          x      <- DomainWire.finite(cross, "x")
          y      <- DomainWire.finite(cross, "y")
          radius <- DomainWire.finite(json, "radiusDegrees")
          policy <- InitialFixationPolicy
            .dropLeadingInClosedDisc(Pt[U](x, y), radius)
            .left
            .map {
              case e @ InitialFixationError.NonPositiveRadius(_) =>
                CodecError.Field("radiusDegrees", json, e.message)
              case e => CodecError.Field("cross", json, e.message)
            }
        yield policy
      case other =>
        Left(
          CodecError.Field("initialFixations", json, s"unknown initial-fixation policy $other")
        )
    }

  def offWindow(policy: OffWindowPolicy): String = policy match
    case OffWindowPolicy.Exclude   => "exclude"
    case OffWindowPolicy.FailTrial => "failTrial"

  def readOffWindow(name: String): Option[OffWindowPolicy] =
    OffWindowPolicy.values.find(p => offWindow(p) == name)

  def scale[U <: Unit2D](value: StudyScale[U]): Json = value match
    case StudyScale.Native(e) =>
      Json.obj("kind" -> Json.fromString("native"), "estimate" -> estimate(e))
    case StudyScale.Angular(e) =>
      Json.obj("kind" -> Json.fromString("degrees"), "estimate" -> estimate(e))

  def readScale[U <: Unit2D](json: Json): Either[CodecError, StudyScale[U]] =
    Wire.field[String](json, "kind").flatMap {
      case "native" =>
        Wire.field[Json](json, "estimate").flatMap(readEstimate[U]).map(StudyScale.Native(_))
      case "degrees" =>
        Wire
          .field[Json](json, "estimate")
          .flatMap(readEstimate[Unit2D.Deg])
          .map(StudyScale.Angular[U](_))
      case other => Left(CodecError.Field("scale", json, s"unknown scale kind $other"))
    }

  def estimate[U <: Unit2D](value: StudyEstimate[U]): Json = value match
    case StudyEstimate.Anisotropic(x, y, edges) =>
      Json.obj(
        "kind"   -> Json.fromString("anisotropic"),
        "sigmaX" -> Json.fromDoubleOrNull(x.value),
        "sigmaY" -> Json.fromDoubleOrNull(y.value),
        "edges"  -> Json.fromString(edges.toString)
      )
    case StudyEstimate.Binned()               => Json.obj("kind" -> Json.fromString("binned"))
    case StudyEstimate.Gaussian(sigma, edges) =>
      Json.obj(
        "kind"  -> Json.fromString("gaussian"),
        "sigma" -> Json.fromDoubleOrNull(sigma.value),
        "edges" -> Json.fromString(edges.toString)
      )

  def readEstimate[U <: Unit2D](json: Json): Either[CodecError, StudyEstimate[U]] =
    Wire.field[String](json, "kind").flatMap {
      case "anisotropic" =>
        for
          rawX  <- Wire.field[Double](json, "sigmaX")
          x     <- Sigma.of[U](rawX).left.map(e => CodecError.Field("sigmaX", json, e.message))
          rawY  <- Wire.field[Double](json, "sigmaY")
          y     <- Sigma.of[U](rawY).left.map(e => CodecError.Field("sigmaY", json, e.message))
          name  <- Wire.field[String](json, "edges")
          edges <- EdgePolicy.values
            .find(_.toString == name)
            .toRight(CodecError.Field("edges", json, s"unknown edge policy $name"))
        yield StudyEstimate.Anisotropic(x, y, edges)
      case "binned"   => Right(StudyEstimate.Binned())
      case "gaussian" =>
        for
          value <- Wire.field[Double](json, "sigma")
          sigma <- Sigma.of[U](value).left.map(e => CodecError.Field("sigma", json, e.message))
          edgesName <- Wire.field[String](json, "edges")
          edges     <- EdgePolicy.values
            .find(_.toString == edgesName)
            .toRight(CodecError.Field("edges", json, s"unknown edge policy $edgesName"))
        yield StudyEstimate.Gaussian(sigma, edges)
      case other => Left(CodecError.Field("estimate", json, s"unknown estimator $other"))
    }
