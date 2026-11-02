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

import eyes4s.codec.{ByteDigest, CanonicalDigest}
import eyes4s.studio.core.backend.{AnalysisRevision, DatasetRevision}
import eyes4s.studio.core.bundle.*
import eyes4s.studio.core.command.*
import eyes4s.studio.core.fixture.StoryMoments
import io.circe.Json

class LocationDocumentSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val example                           = get(StoryMoments.t1)
  private val timed    = example.timedDatasets.head.copy(id = DatasetRevision(1), parent = None)
  private val analysis =
    example.timedAnalyses.head.copy(id = AnalysisRevision(1), dataset = timed.id)
  private val input   = get(CanonicalDigest.parse[LocationInputArtifact]("1" * 64))
  private val sources = get(LocationBundleSources.of(LocationBundleSources.roles.map { role =>
    Source(
      role,
      get(SourcePath.of(s"figrim/${role.label.replace(' ', '-')}.txt")),
      ByteDigest.sha256(BundleSamples.utf8(role.label)),
      None
    )
  }))
  private val locations = LocationDatasetRevision(
    DatasetRevision(2),
    None,
    sources,
    LocationProfile.FigrimV1,
    LocationAdmissionDecision.Admitted(input)
  )
  private val locationAnalysis = LocationAnalysisRevision(
    AnalysisRevision(2),
    locations.id,
    CoreBinding.unbound,
    LocationRecipe(LocationProfile.FigrimV1, CoreBinding.Bound(input)),
    get(RevisionName.of("FIGRIM locations")),
    "Uniform pixel histograms; observer slots are not people"
  )
  private def document(
      datasets: Vector[DatasetDefinition] =
        Vector(timed, locations, timed.copy(id = DatasetRevision(3))),
      analyses: Vector[AnalysisDefinition] = Vector(
        analysis,
        locationAnalysis,
        analysis.copy(id = AnalysisRevision(3), dataset = DatasetRevision(3))
      ),
      draft: Option[Draft] = None
  ) = StudioDocument.of(
    datasets,
    analyses,
    draft,
    Vector.empty,
    Vector.empty,
    Vector.empty,
    PresentationState.default,
    Vector.empty
  )
  private val mixed                         = get(document())
  private def encode(value: StudioDocument) = get(
    ProjectBundle.encode(value, SharingOptions.complete, BundleSamples.inputsFor(value))
  )

  test("mixed definitions round trip without mappings or timing for spatial data") {
    val envelope = get(StudioDocument.encode(mixed))
    assertEquals(envelope.hcursor.downField("schema").get[Int]("version"), Right(13))
    assertEquals(get(StudioDocument.decode(envelope)), mixed)
    val saved   = encode(mixed)
    val spatial = saved.manifest.parts.datasets(1)
    assert(spatial.isInstanceOf[LocationDatasetPart])
    val files    = saved.parts.toMap
    val reopened = get(ProjectBundle.assemble(saved.manifest, path => Right(files(path))))
    assertEquals(reopened.document, mixed)
    assertEquals(reopened.document.datasets.map(_.id.number), Vector(1, 2, 3))
    assertEquals(reopened.document.analyses.map(_.id.number), Vector(1, 2, 3))
    assertEquals(reopened.document.datasets(1).sourceFiles, sources.entries)
    assertEquals(mixed.nextAnalysisId, Right(AnalysisRevision(4)))
    assertEquals(mixed.familyOf(locationAnalysis.id), Some(AnalysisFamilyId.Legacy))
  }

  test("all older document, science and manifest versions refuse spatial definitions") {
    val documentLadder = get(StudioDocument.ladder)
    val docJson = get(StudioDocument.encode(mixed)).hcursor.get[Json]("value").toOption.get
    documentLadder.versions.dropRight(1).foreach { version =>
      assert(documentLadder.writeAt(version, mixed).isLeft, version.toString)
      assert(documentLadder.readAt(version, docJson).isLeft, version.toString)
    }
    val scienceLadder = get(ScienceContent.ladder)
    val scienceJson   = get(get(ScienceContent.codec).encode(mixed.science)).hcursor
      .get[Json]("value")
      .toOption
      .get
    scienceLadder.versions.dropRight(1).foreach { version =>
      assert(scienceLadder.writeAt(version, mixed.science).isLeft, version.toString)
      assert(scienceLadder.readAt(version, scienceJson).isLeft, version.toString)
    }
    val manifest       = encode(mixed).manifest
    val manifestLadder = get(ProjectManifest.ladder)
    val manifestJson   =
      get(get(ProjectManifest.codec).encode(manifest)).hcursor.get[Json]("value").toOption.get
    manifestLadder.versions.dropRight(1).foreach { version =>
      assert(manifestLadder.writeAt(version, manifest).isLeft, version.toString)
      assert(manifestLadder.readAt(version, manifestJson).isLeft, version.toString)
    }
  }

  test("typed data, parent, recipe input and timed draft cannot cross scientific kinds") {
    assert(document(analyses = Vector(analysis.copy(dataset = locations.id))).isLeft)
    assert(document(analyses = Vector(locationAnalysis.copy(dataset = timed.id))).isLeft)
    assert(document(datasets = Vector(timed, locations.copy(parent = Some(timed.id)))).isLeft)
    val other = get(CanonicalDigest.parse[LocationInputArtifact]("2" * 64))
    assert(
      document(analyses =
        Vector(
          locationAnalysis.copy(recipe =
            locationAnalysis.recipe.copy(input = CoreBinding.Bound(other))
          )
        )
      ).isLeft
    )
    val rebased =
      get(Draft.of(AnalysisRevision(4), analysis.id, Some(locations.id), Vector.empty))
    assert(document(draft = Some(rebased)).isLeft)
  }

  test(
    "timed edits, undo, redo and journal replay preserve the interleaved spatial definitions"
  ) {
    val command = Command.PutReporting(ReportingSpec.overall)
    val changed = get(History.start(mixed).apply(command)).history
    val undone  = get(changed.undo).history
    val redone  = get(undone.redo).history
    assertEquals(undone.document, mixed)
    assertEquals(redone.document.datasets, mixed.datasets)
    assertEquals(redone.document.analyses, mixed.analyses)
    val entries = Vector(JournalEntry.Apply(command), JournalEntry.Undo, JournalEntry.Redo)
    val lines   = Vector(get(CommandJournal.start(mixed))) ++ entries.zipWithIndex.map(
      (entry, i) => get(CommandJournal.entry(i + 1, entry))
    ) :+ get(CommandJournal.checkpoint(3, redone.document))
    assertEquals(
      get(CommandJournal.replay(mixed, lines.mkString("\n") + "\n")).history.document,
      redone.document
    )
  }

  test("spatial sources require all five roles and never satisfy the timed source parser") {
    assert(LocationBundleSources.of(sources.entries.dropRight(1)).isLeft)
    assert(LocationBundleSources.of(sources.entries :+ sources.entries.head).isLeft)
    assert(
      LocationBundleSources
        .of(
          sources.entries.updated(1, sources.entries(1).copy(path = sources.entries.head.path))
        )
        .isLeft
    )
    assert(Sources.of(timed.sources.entries ++ sources.entries).isLeft)
    val saved = encode(mixed)
    assert(
      ProjectBundle
        .encode(
          mixed,
          SharingOptions.complete,
          saved.manifest.inputs.filterNot(_.kind == InputKind.Source(SourceRole.LocationLedger))
        )
        .isLeft
    )
  }

  test("spatial image relinks survive bundle extraction with their owning revision") {
    import eyes4s.studio.core.assets.{AssetFile, AssetRef}
    val file  = get(AssetFile.of("image.png"))
    val ref   = AssetRef(file, ByteDigest.sha256(BundleSamples.utf8("image bytes")))
    val value = get(mixed.withRelinks(AssetRelinks.empty.set(locations.id, file, Some(ref))))
    val saved = encode(value)
    val files = saved.parts.toMap
    assertEquals(
      get(ProjectBundle.assemble(saved.manifest, path => Right(files(path)))).document,
      value
    )
  }

  test("manifest part kind must agree with the dataset payload in both directions") {
    val saved       = encode(mixed)
    val files       = saved.parts.toMap
    val timedPart   = saved.manifest.parts.datasets.head.asInstanceOf[DatasetParts]
    val spatialPart = saved.manifest.parts.datasets(1)
    def changed(parts: Vector[DatasetPartDefinition]) = get(
      ProjectManifest.of(
        saved.manifest.document,
        saved.manifest.science,
        saved.manifest.sharing,
        saved.manifest.inputs,
        saved.manifest.parts.copy(datasets = parts),
        saved.manifest.presentation
      )
    )
    // The replacement mapping must be unique in the manifest before assembly.
    val spatialOnly = changed(Vector(DatasetParts(spatialPart.dataset, timedPart.mapping)))
    assert(ProjectBundle.assemble(spatialOnly, path => Right(files(path))).left.exists {
      case BundleError.MalformedPart(_, reason) => reason.contains("kind")
      case _                                    => false
    })
    val timedAsSpatial = changed(Vector(LocationDatasetPart(timedPart.dataset)))
    assert(ProjectBundle.assemble(timedAsSpatial, path => Right(files(path))).left.exists {
      case BundleError.MalformedPart(_, reason) => reason.contains("kind")
      case _                                    => false
    })
  }

  test("unreferenced spatial inputs still require the new manifest version") {
    val legacy = get(document(datasets = Vector(timed), analyses = Vector(analysis)))
    val saved  = encode(legacy)
    val source = sources.entries.head
    val entry  = get(
      InputEntry.of(
        InputKind.Source(source.role),
        source.path.value.split('/').last,
        source.bytes,
        0L
      )
    )
    val manifest = get(
      ProjectManifest.of(
        saved.manifest.document,
        saved.manifest.science,
        saved.manifest.sharing,
        saved.manifest.inputs :+ entry,
        saved.manifest.parts,
        saved.manifest.presentation
      )
    )
    val ladder = get(ProjectManifest.ladder)
    val raw    =
      get(get(ProjectManifest.codec).encode(manifest)).hcursor.get[Json]("value").toOption.get
    ladder.versions.dropRight(1).foreach { version =>
      assert(ladder.writeAt(version, manifest).isLeft)
      assert(ladder.readAt(version, raw).isLeft)
    }
  }
