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

import eyes4s.codec.{CodecError, SchemaLadder}
import eyes4s.studio.core.backend.{AnalysisRevision, DatasetRevision, JobId, RunId}
import io.circe.syntax.*
import io.circe.{Decoder, Encoder, Json}
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

/** The document's wire contract (ticket S2.1): every type round-trips, every
  * sample encodes exactly as pinned, decoding validates again, the writer is
  * canonical, and a newer schema version is reached through CR3's ladder.
  */
class StudioDocumentCodecSuite extends munit.ScalaCheckSuite:
  import DocumentGen.*

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(50)

  private def roundTrip[A: Encoder: Decoder](name: String, gen: Gen[A]): Unit =
    property(s"$name round-trips") {
      forAll(gen) { (a: A) =>
        assertEquals(io.circe.parser.decode[A](a.asJson.noSpaces), Right(a))
      }
    }

  roundTrip("CoreBinding", binding[StudyPlanArtifact])
  roundTrip("SourcePath", sourcePath)
  roundTrip("SemanticIdentity", semantic)
  roundTrip("Source", source(SourceRole.Trials))
  roundTrip("Sources", sources)
  roundTrip("ColumnMapping", mapping)
  roundTrip("DeclaredUnits", units)
  roundTrip("Geometry", geometry)
  roundTrip("CoordinateCorrection", correction)
  roundTrip("CorrectionTarget", target)
  roundTrip("AdmissionChoice", admission)
  roundTrip("AdmissionDecision", decision)
  roundTrip("DatasetRevisionSpec", dataset(3, Vector(1, 2)))
  roundTrip("Sigma", sigma)
  roundTrip("ScaleSet", scales)
  roundTrip("GridSize", grid)
  roundTrip("InitialFixationChoice", initial)
  roundTrip("MatchedChoice", matched)
  roundTrip("Recipe", recipe)
  roundTrip("RecipeChange", change)
  roundTrip("Draft", analysis(4, Vector(1)).flatMap(a => draft(a, 5)).suchThat(_.nonEmpty))
  roundTrip("StudioFields", studio)
  roundTrip("AnalysisRevisionSpec", analysis(4, Vector(1, 2)))
  roundTrip("RunLifecycle", lifecycle)
  roundTrip("ReportingFilter", filter)
  roundTrip("ReportingSpec", reporting("spec"))
  roundTrip("PanelSpec", scales.flatMap(panels).map(_.head))
  roundTrip("PresentationState", presentation(Vector(RunId(1), RunId(2))))
  roundTrip("StudioDocument", document)

  property("every generated document round-trips through the versioned codec") {
    forAll(document) { (d: StudioDocument) =>
      assertEquals(
        StudioDocument.encode(d).flatMap(j => StudioDocument.parse(j.noSpaces)),
        Right(d)
      )
    }
  }

  property("the writer is canonical: every object's keys ascend") {
    def sorted(j: Json): Boolean = j.fold(
      true,
      _ => true,
      _ => true,
      _ => true,
      _.forall(sorted),
      o => o.keys.toVector == o.keys.toVector.sorted && o.values.forall(sorted)
    )
    forAll(document) { (d: StudioDocument) =>
      assert(StudioDocument.encode(d).exists(sorted))
    }
  }

  test("every sample round-trips") {
    assertEquals(DocumentSamples.all.flatMap(_.roundTrips.left.toOption), Vector.empty)
  }

  test("every sample encodes exactly as pinned") {
    val actual = DocumentSamples.all.map(s => s.name -> s.json.noSpaces).toMap ++ Map(
      "document.t1" -> StudioDocument
        .encode(DocumentSamples.t1)
        .fold(_.message, _.noSpaces)
    )
    // Pins compare as JSON values: Scala.js prints the double 2.0 as `2`.
    def same(pin: String, json: String) =
      io.circe.parser.parse(pin) == io.circe.parser.parse(json)
    val drift =
      actual.toVector
        .sortBy(_._1)
        .filter((n, j) => !DocumentPins.pins.get(n).exists(same(_, j)))
    // A deliberate schema change updates DocumentPins from these lines.
    drift.foreach((n, j) => println(s"PIN\t$n\t$j"))
    assertEquals(drift.map(_._1), Vector.empty)
    assertEquals(DocumentPins.pins.keySet, actual.keySet)
    assertEquals(DocumentSamples.all.map(_.name).distinct.size, DocumentSamples.all.size)
  }

  test("the document envelope carries its schema name and version") {
    val json = StudioDocument.encode(DocumentSamples.t2)
    assertEquals(
      json.map(
        _.hcursor
          .downField("schema")
          .as[(String, Int)](using
            Decoder.forProduct2("name", "version")((n: String, v: Int) => (n, v))
          )
      ),
      // t2's admitted revisions record their admission policy: version 3 (S5.6).
      Right(Right(("studio.document", 3)))
    )
    val future = json.map(
      _.deepMerge(
        Json.obj(
          "schema" -> Json.obj(
            "name"    -> "studio.document".asJson,
            "version" -> 9.asJson
          )
        )
      )
    )
    assert(future.flatMap(StudioDocument.decode).isLeft, future)
  }

  test("a pinned document upcasts through a later CR3 ladder version") {
    // A hypothetical version 6 that adds a `notes` member. Version 3 expresses
    // every value of t1, so the codec still writes version 3 for it (it records
    // an admission policy, S5.6); `lift` rewrites it as version 6 with the same
    // meaning.
    val v5                               = StudioDocument.ladder.toOption.get
    val v6: SchemaLadder[StudioDocument] =
      v5.next(
        _ => true,
        json => json.deepMerge(Json.obj("notes" -> Json.arr()))
      )(d =>
        StudioDocument
          .encode(d)
          .map(
            _.hcursor.downField("value").focus.get.deepMerge(Json.obj("notes" -> Json.arr()))
          )
      )(json => v5.readAt(v5.latest, json.mapObject(_.remove("notes"))))
    val pinned = io.circe.parser.parse(DocumentPins.pins("document.t1")).toOption.get
    val lifted = v6.lift(pinned)
    assertEquals(
      lifted.map(_.hcursor.downField("schema").downField("version").as[Int]),
      Right(Right(6))
    )
    assertEquals(lifted.flatMap(v6.codec.decode), Right(DocumentSamples.t1))
    assertEquals(v6.codec.encode(DocumentSamples.t1), Right(pinned))
    assertEquals(
      v6.versions.map(v => (v.name, v.version)),
      (1 to 6).toVector.map(("studio.document", _))
    )
  }

  // --- Version 2: the trial inventory mapping (S5.4) --------------------------

  private val ladder = StudioDocument.ladder.toOption.get
  private val ids    = StudioSchemaIds.ids.toOption.get

  /** `decision` with no recorded admission policy, as before S5.6. */
  private def unrecorded(decision: AdmissionDecision): AdmissionDecision = decision match
    case AdmissionDecision.Admitted(_, l, i) => AdmissionDecision.Admitted(None, l, i)
    case other                               => other

  private def withDatasets(f: DatasetRevisionSpec => DatasetRevisionSpec) =
    val t1 = DocumentSamples.t1
    StudioDocument
      .of(
        t1.datasets.map(f),
        t1.analyses,
        t1.draft,
        t1.runs,
        t1.reporting,
        t1.figures,
        t1.presentation,
        t1.jobs
      )
      .toOption
      .get

  /** t1 as a pre-S5.6 build held it: no admission policy recorded. */
  private val t1WithoutPolicy = withDatasets(d => d.copy(decision = unrecorded(d.decision)))

  /** t1 as a pre-S5.4 build held it: no dataset maps its inventory. */
  private val t1WithoutInventory =
    val t1 = t1WithoutPolicy
    StudioDocument
      .of(
        t1.datasets.map(_.copy(inventory = None)),
        t1.analyses,
        t1.draft,
        t1.runs,
        t1.reporting,
        t1.figures,
        t1.presentation,
        t1.jobs
      )
      .toOption
      .get

  private def version(json: Either[?, Json]) =
    json.map(_.hcursor.downField("schema").downField("version").as[Int])

  test("a version-1 document from before S5.4 loads, and lifts to version 5 unchanged") {
    assertEquals(
      ladder.versions.map(v => (v.name, v.version)),
      (1 to 5).toVector.map(("studio.document", _))
    )
    assertEquals(ladder.versions.head, ids.document)
    val v1 = io.circe.parser.parse(DocumentPins.t1BeforeInventory).toOption.get
    assertEquals(StudioDocument.decode(v1), Right(t1WithoutInventory))
    // It re-encodes to its own version-1 bytes: nothing in it needs version 2.
    assertEquals(StudioDocument.encode(t1WithoutInventory), Right(v1))
    // The upcast is the identity on the payload; only the schema moves.
    val lifted = ladder.lift(v1)
    assertEquals(version(lifted), Right(Right(5)))
    assertEquals(
      lifted.map(_.hcursor.downField("value").focus),
      Right(v1.hcursor.downField("value").focus)
    )
    assertEquals(lifted.flatMap(StudioDocument.decode), Right(t1WithoutInventory))
  }

  test("a document with an inventory mapping is version 2 and round-trips") {
    val encoded = StudioDocument.encode(t1WithoutPolicy)
    assertEquals(version(encoded), Right(Right(2)))
    assertEquals(encoded.flatMap(StudioDocument.decode), Right(t1WithoutPolicy))
    assert(encoded.exists(_.noSpaces.contains("\"inventory\"")))
    assertEquals(ladder.earliest(t1WithoutPolicy).version, 2)
    assertEquals(ladder.earliest(t1WithoutInventory).version, 1)
  }

  test("a version-1 reader refuses a version-2 document, never dropping the mapping") {
    val older   = ladder.upTo(ids.document).toOption.get
    val encoded = StudioDocument.encode(t1WithoutPolicy).toOption.get
    assertEquals(
      older.codec.decode(encoded),
      Left(
        CodecError.UnsupportedSchema(
          "studio document",
          ladder.versions(1),
          Vector(ids.document)
        )
      )
    )
    // Its own writer cannot hold the mapping: the version-1 payload drops it.
    val v1Datasets = older
      .writeAt(ids.document, DocumentSamples.t1)
      .map(_.hcursor.downField("datasets").focus.flatMap(_.asArray).getOrElse(Vector.empty))
    assertEquals(v1Datasets.map(_.size), Right(2))
    assert(v1Datasets.exists(_.forall(_.asObject.exists(!_.contains("inventory")))))
  }

  // --- Version 3: the admission policy (S5.6) ---------------------------------------

  private def policies(json: Json): Vector[Option[Json]] =
    json.hcursor
      .downField("value")
      .downField("datasets")
      .focus
      .flatMap(_.asArray)
      .getOrElse(Vector.empty)
      .map(_.hcursor.downField("decision").downField("Admitted").downField("policy").focus)

  test("a version-2 document from before S5.6 loads, and lifts to version 5 unchanged") {
    val v2 = io.circe.parser.parse(DocumentPins.t1BeforePolicy).toOption.get
    assertEquals(version(Right(v2)), Right(Right(2)))
    assertEquals(StudioDocument.decode(v2), Right(t1WithoutPolicy))
    assertEquals(
      t1WithoutPolicy.datasets.map(_.decision.admittedUnder),
      Vector(None, None)
    )
    // It re-encodes to its own version-2 bytes: nothing in it needs version 3.
    assertEquals(StudioDocument.encode(t1WithoutPolicy), Right(v2))
    val lifted = ladder.lift(v2)
    assertEquals(version(lifted), Right(Right(5)))
    assertEquals(
      lifted.map(_.hcursor.downField("value").focus),
      Right(v2.hcursor.downField("value").focus)
    )
    assertEquals(lifted.flatMap(StudioDocument.decode), Right(t1WithoutPolicy))
  }

  test("a document that records an admission policy is version 3 and round-trips") {
    val t1      = DocumentSamples.t1
    val encoded = StudioDocument.encode(t1)
    assertEquals(version(encoded), Right(Right(3)))
    assertEquals(encoded.flatMap(StudioDocument.decode), Right(t1))
    assertEquals(
      encoded.map(policies),
      Right(Vector(Some(Json.obj("ReviewExclusions" -> Json.obj())), None))
    )
    assertEquals(ladder.earliest(t1).version, 3)
  }

  test("a version-2 reader refuses a version-3 document, never dropping the policy") {
    val older   = ladder.upTo(ladder.versions(1)).toOption.get
    val encoded = StudioDocument.encode(DocumentSamples.t1).toOption.get
    assertEquals(
      older.codec.decode(encoded),
      Left(
        CodecError.UnsupportedSchema(
          "studio document",
          ladder.versions(2),
          ladder.versions.take(2)
        )
      )
    )
    // Its own writer cannot hold the policy: the version-2 payload drops it,
    // and the version-1 payload drops it too.
    val v2Payload = older.writeAt(ladder.versions(1), DocumentSamples.t1)
    assertEquals(
      v2Payload.map(p => policies(Json.obj("value" -> p))),
      Right(Vector(None, None))
    )
    val v1Payload = older.writeAt(ids.document, DocumentSamples.t1)
    assertEquals(
      v1Payload.map(p => policies(Json.obj("value" -> p))),
      Right(Vector(None, None))
    )
  }

  // --- Version 5: the PerceptionImagery preset (S7.1) ---------------------------------

  private val imagery = StudioDocument
    .of(
      DocumentSamples.t1.datasets,
      DocumentSamples.t1.analyses.map(a =>
        a.copy(studio = a.studio.copy(preset = Preset.PerceptionImagery))
      ),
      DocumentSamples.t1.draft,
      DocumentSamples.t1.runs,
      DocumentSamples.t1.reporting,
      DocumentSamples.t1.figures,
      DocumentSamples.t1.presentation,
      DocumentSamples.t1.jobs
    )
    .toOption
    .get

  test("a document naming the PerceptionImagery preset is version 5 and round-trips") {
    val encoded = StudioDocument.encode(imagery)
    assertEquals(version(encoded), Right(Right(5)))
    assertEquals(encoded.flatMap(StudioDocument.decode), Right(imagery))
    assertEquals(ladder.earliest(imagery).version, 5)
    // Without it, the document keeps its earlier version.
    assertEquals(ladder.earliest(DocumentSamples.t1).version, 3)
  }

  test("a version-4 reader refuses a version-5 document; no earlier rung names the preset") {
    val older   = ladder.upTo(ladder.versions(3)).toOption.get
    val encoded = StudioDocument.encode(imagery).toOption.get
    assertEquals(
      older.codec.decode(encoded),
      Left(
        CodecError.UnsupportedSchema(
          "studio document",
          ladder.versions(4),
          ladder.versions.take(4)
        )
      )
    )
    // A preset cannot be dropped as a member can: every earlier writer
    // refuses the document, and a version-4 envelope naming it is refused.
    val refusal = CodecError.Unsupported("studio document", "a preset needs version 5")
    ladder.versions.take(4).foreach { v =>
      assertEquals(older.writeAt(v, imagery), Left(refusal), s"$v")
    }
    val asV4 = encoded.hcursor
      .downField("schema")
      .downField("version")
      .withFocus(_ => Json.fromInt(4))
      .top
      .get
    assertEquals(StudioDocument.decode(asV4), Left(refusal))
  }

  test("decoding re-validates every value and every cross-reference") {
    val doc = StudioDocument.encode(DocumentSamples.t2).toOption.get
    def refused(patch: Json => Json): Unit =
      val broken = patch(doc)
      assert(StudioDocument.decode(broken).isLeft, broken.noSpaces.take(200))
    def value(f: Json => Json)(j: Json): Json =
      j.mapObject(o => o.add("value", f(o("value").get)))
    // A run naming an analysis revision the document does not have.
    refused(
      value(
        _.hcursor
          .downField("runs")
          .downN(0)
          .downField("analysis")
          .withFocus(_ => 9.asJson)
          .top
          .get
      )
    )
    // A draft whose change does not start from its base's recipe.
    refused(
      value(
        _.hcursor
          .downField("draft")
          .downField("changes")
          .downN(0)
          .downField("Scales")
          .downField("before")
          .withFocus(_ => Json.arr(1.asJson))
          .top
          .get
      )
    )
    // A negative scale.
    refused(
      value(
        _.hcursor
          .downField("analyses")
          .downN(0)
          .downField("recipe")
          .downField("scales")
          .withFocus(_ => Json.arr((-1).asJson))
          .top
          .get
      )
    )
    // A source digest that is not SHA-256.
    refused(
      value(
        _.hcursor
          .downField("datasets")
          .downN(0)
          .downField("sources")
          .downN(0)
          .downField("bytes")
          .withFocus(_ => "abc".asJson)
          .top
          .get
      )
    )
    // The presentation shows a run that does not exist.
    refused(
      value(
        _.hcursor
          .downField("presentation")
          .downField("shownRun")
          .withFocus(_ => 99.asJson)
          .top
          .get
      )
    )
  }

  test("smart constructors name their operands") {
    assertEquals(
      SourcePath.of("../x.csv"),
      Left(DocumentError.BadPath("../x.csv", "it has an empty, '.' or '..' segment"))
    )
    assertEquals(Sigma.of(0.0), Left(DocumentError.NotPositive("sigma (degrees)", 0.0)))
    assertEquals(
      ColumnMapping.of(Vector.empty).left.map(_.message),
      Left(
        "The column mapping has no participant, trial, ordinal, sample count, x, y, onset, " +
          "duration column."
      )
    )
    val screen = right(ScreenSize.of(1920, 1080))
    val image  = right(ImagePlacement.of(1000, 0, 1024, 768))
    assertEquals(
      Geometry.of(screen, image, right(DeclaredPixelsPerDegree.of(35.0))),
      Left(DocumentError.PlacementOffScreen(image, screen))
    )
    val t2   = DocumentSamples.t2
    val rev4 = t2.analyses(1)
    val base = rev4.recipe
    assertEquals(
      Draft.of(AnalysisRevision(5), rev4.id, None, Vector.empty),
      Left(DocumentError.NoChanges(AnalysisRevision(5), rev4.id))
    )
    val same = RecipeChange.Grid(base.grid, base.grid)
    assertEquals(
      Draft.of(AnalysisRevision(5), rev4.id, None, Vector(same)),
      Left(DocumentError.IdentityChange(AnalysisRevision(5), RecipeField.Grid, "64×48"))
    )
    val wrong = RecipeChange.Grid(right(GridSize.of(8, 8)), right(GridSize.of(16, 16)))
    assertEquals(
      Draft.against(AnalysisRevision(5), rev4, None, Vector(wrong)).left.map(_.message),
      Left("Draft rev 5 changes the grid from 8×8, but rev 4 has 64×48.")
    )
    assertEquals(
      Draft
        .between(AnalysisRevision(4), rev4, base.copy(grid = right(GridSize.of(8, 8))))
        .flatMap(d =>
          StudioDocument.of(
            t2.datasets,
            t2.analyses,
            Some(d),
            t2.runs,
            t2.reporting,
            t2.figures,
            t2.presentation,
            t2.jobs
          )
        ),
      Left(DocumentError.DraftNotLatest(AnalysisRevision(4), rev4.id))
    )
  }

  test("every eyes4s StudyField has a recipe field and a change case") {
    val core = eyes4s.plan.StudyField.values.toVector
    assertEquals(core.map(RecipeField.of(_).core), core)
    assertEquals(RecipeField.values.toVector.map(_.core), core)
    val t2    = DocumentSamples.t2
    val base  = t2.analyses(1).recipe
    val other = DocumentSamples.all.collect {
      case s if s.name.startsWith("change.") => s.value
    }
    val fields = other.collect { case c: RecipeChange => c.field }
    assertEquals(fields.toSet, RecipeField.values.toSet)
    other.collect { case c: RecipeChange => c }.foreach { c =>
      assertEquals(c.mismatch(base), None, c.render)
      assertEquals(RecipeChange.between(base, c.applyTo(base)), Vector(c))
      assertEquals(c.inverse.applyTo(c.applyTo(base)), base)
    }
  }

  test("a draft may rebase onto a newly admitted dataset, never onto its own or a draft") {
    val t2     = DocumentSamples.t2
    val t1     = DocumentSamples.t1
    val rev3   = t2.analyses(0)
    val r2     = DatasetRevision(2)
    val r3     = DatasetRevision(3)
    val rebase = Draft.between(AnalysisRevision(5), rev3, rev3.recipe, Some(r3))
    assertEquals(rebase.map(d => (d.changeCount, d.render)), Right((1, "data → r3")))
    assertEquals(
      Draft.between(AnalysisRevision(5), rev3, rev3.recipe, Some(r2)),
      Left(DocumentError.RebaseToSame(AnalysisRevision(5), r2))
    )
    // In t1, r3 is still pending admission.
    assertEquals(
      Draft
        .between(AnalysisRevision(4), t1.analyses(0), t1.analyses(0).recipe, Some(r3))
        .flatMap(d =>
          StudioDocument.of(
            t1.datasets,
            t1.analyses,
            Some(d),
            t1.runs,
            t1.reporting,
            t1.figures,
            t1.presentation,
            t1.jobs
          )
        ),
      Left(DocumentError.RebaseNotAdmitted(AnalysisRevision(4), r3))
    )
  }

  test("job handles name one running run each") {
    val t3 = DocumentSamples.t3
    assertEquals(
      t3.withJobs(Vector(JobHandle(RunId(7), JobId(2)))),
      Left(DocumentError.JobNotRunning(RunId(7), JobId(2)))
    )
    assertEquals(
      t3.withJobs(Vector(JobHandle(RunId(8), JobId(1)), JobHandle(RunId(8), JobId(2)))),
      Left(DocumentError.DuplicateJobs(RunId(8), Vector(JobId(1), JobId(2))))
    )
    assert(t3.withJobs(Vector.empty).isRight)
  }

  test("reporting filters are a canonical set") {
    val response = right(Covariate.of("response"))
    val keep     =
      ReportingFilter.Keep(response, right(ValueSet.of(response, Vector("Remembered"))))
    val out = ReportingFilter.OutsideWindowAtMost(right(Share.of(0.25)))
    def spec(fs: Vector[ReportingFilter]) =
      ReportingSpec.of(
        right(ReportingId.of("s")),
        "S",
        None,
        fs,
        None,
        ReportingWeight.ParticipantMeans
      )
    assertEquals(spec(Vector(out, keep, out)), spec(Vector(keep, out)))
    assertEquals(spec(Vector(out, keep)).map(_.filters), Right(Vector(keep, out)))
  }
