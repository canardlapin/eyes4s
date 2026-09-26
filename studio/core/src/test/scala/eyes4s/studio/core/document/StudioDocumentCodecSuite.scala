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

import eyes4s.codec.SchemaLadder
import eyes4s.plan.DefinitionId
import eyes4s.studio.core.backend.{AnalysisRevision, RunId}
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
      val codec = StudioDocument.codec
      assertEquals(codec.encode(d).flatMap(j => codec.parse(j.noSpaces)), Right(d))
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
      assert(StudioDocument.codec.encode(d).exists(sorted))
    }
  }

  test("every sample round-trips") {
    assertEquals(DocumentSamples.all.flatMap(_.roundTrips.left.toOption), Vector.empty)
  }

  test("every sample encodes exactly as pinned") {
    val actual = DocumentSamples.all.map(s => s.name -> s.json.noSpaces).toMap ++ Map(
      "document.t1" -> StudioDocument.codec
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
    val json = StudioDocument.codec.encode(DocumentSamples.t2)
    assertEquals(
      json.map(
        _.hcursor
          .downField("schema")
          .as[(String, Int)](using
            Decoder.forProduct2("name", "version")((n: String, v: Int) => (n, v))
          )
      ),
      Right(Right(("eyes4s.studio.document", 1)))
    )
    val future = json.map(
      _.deepMerge(
        Json.obj(
          "schema" -> Json.obj(
            "name"    -> "eyes4s.studio.document".asJson,
            "version" -> 9.asJson
          )
        )
      )
    )
    assert(future.flatMap(StudioDocument.codec.decode).isLeft, future)
  }

  test("a pinned version-1 document upcasts through a later CR3 ladder version") {
    // A hypothetical version 2 that adds a `notes` member. Version 1 expresses
    // every value, so the codec still writes version 1; `lift` rewrites a
    // stored version-1 document as version 2 with the same meaning.
    val v2: SchemaLadder[StudioDocument] =
      StudioDocument.ladder.next(
        _ => true,
        json => json.deepMerge(Json.obj("notes" -> Json.arr()))
      )(d =>
        StudioDocument.codec
          .encode(d)
          .map(
            _.hcursor.downField("value").focus.get.deepMerge(Json.obj("notes" -> Json.arr()))
          )
      )(json =>
        StudioDocument.ladder.readAt(
          StudioDocumentSchemas.document,
          json.mapObject(_.remove("notes"))
        )
      )
    val pinned = io.circe.parser.parse(DocumentPins.pins("document.t1")).toOption.get
    val lifted = v2.lift(pinned)
    assertEquals(
      lifted.map(_.hcursor.downField("schema").downField("version").as[Int]),
      Right(Right(2))
    )
    assertEquals(lifted.flatMap(v2.codec.decode), Right(DocumentSamples.t1))
    assertEquals(v2.codec.encode(DocumentSamples.t1), Right(pinned))
    assertEquals(
      v2.versions,
      Vector(StudioDocumentSchemas.document, DefinitionId.builtIn("eyes4s.studio.document", 2))
    )
  }

  test("decoding re-validates every value and every cross-reference") {
    val doc = StudioDocument.codec.encode(DocumentSamples.t2).toOption.get
    def refused(patch: Json => Json): Unit =
      val broken = patch(doc)
      assert(StudioDocument.codec.decode(broken).isLeft, broken.noSpaces.take(200))
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
      Draft.of(AnalysisRevision(5), rev4.id, Vector.empty),
      Left(DocumentError.NoChanges(AnalysisRevision(5), rev4.id))
    )
    val same = RecipeChange.Grid(base.grid, base.grid)
    assertEquals(
      Draft.of(AnalysisRevision(5), rev4.id, Vector(same)),
      Left(DocumentError.IdentityChange(AnalysisRevision(5), RecipeField.Grid, "64×48"))
    )
    val wrong = RecipeChange.Grid(right(GridSize.of(8, 8)), right(GridSize.of(16, 16)))
    assertEquals(
      Draft.against(AnalysisRevision(5), rev4, Vector(wrong)).left.map(_.message),
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
            t2.presentation
          )
        ),
      Left(DocumentError.DraftNotLatest(AnalysisRevision(4), rev4.id))
    )
  }
