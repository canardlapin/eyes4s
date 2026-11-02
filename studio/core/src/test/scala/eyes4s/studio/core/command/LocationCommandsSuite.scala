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

package eyes4s.studio.core.command

import eyes4s.codec.{ByteDigest, CanonicalDigest}
import eyes4s.studio.core.backend.{AnalysisRevision, DatasetRevision}
import eyes4s.studio.core.document.*
import eyes4s.studio.core.fixture.StoryMoments

class LocationCommandsSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A =
    value.fold(error => fail(error.toString), identity)

  private val base         = get(StoryMoments.t1)
  private val nextDataset  = DatasetRevision(base.datasets.lastOption.fold(1)(_.id.number + 1))
  private val nextAnalysis = get(base.nextAnalysisId)
  private val input        = get(CanonicalDigest.parse[LocationInputArtifact]("1" * 64))
  private val plan         = get(CanonicalDigest.parse[LocationPlanArtifact]("2" * 64))
  private val sources = get(LocationBundleSources.of(LocationBundleSources.roles.map { role =>
    Source(
      role,
      get(SourcePath.of(s"figrim/${role.label.replace(' ', '-')}.txt")),
      ByteDigest.sha256(IArray.from(role.label.getBytes)),
      None
    )
  }))
  private val append = Command.AppendLocationDataset(
    None,
    sources,
    LocationProfile.FigrimV1,
    input
  )
  private val family = get(AnalysisFamily.of(get(base.nextFamilyId), "FIGRIM map"))
  private val name   = get(RevisionName.of("FIGRIM spatial comparison"))

  private def save(dataset: DatasetRevision) =
    Command.SaveLocationAnalysis(
      family,
      dataset,
      plan,
      name,
      "Fixed-profile location comparison"
    )

  test("append, direct spatial save, reserve, undo and redo preserve mixed definitions") {
    val appended = get(History.start(base).apply(append)).history
    val dataset  = nextDataset
    val saved    = get(appended.apply(save(dataset))).history
    val analysis = nextAnalysis
    val location = saved.document
      .analysisDefinition(analysis)
      .collect { case value: LocationAnalysisRevision =>
        value
      }
      .getOrElse(fail("missing location analysis"))
    assertEquals(location.dataset, dataset)
    assertEquals(location.plan, CoreBinding.Bound(plan))
    assertEquals(
      location.recipe,
      LocationRecipe(LocationProfile.FigrimV1, CoreBinding.Bound(input))
    )
    assertEquals(saved.document.familyOf(analysis), Some(family.id))
    assertEquals(
      saved.document.datasets.map(_.kind),
      base.datasets.map(_.kind) :+ StudyKind.Locations
    )

    val undone = get(saved.undo).history
    assertEquals(undone.document.analysisDefinition(analysis), None)
    val redone = get(undone.redo).history
    assertEquals(redone.document.analysisDefinition(analysis), Some(location))

    val reserved = get(redone.apply(Command.ReserveLocationRun(analysis)))
    val run      = reserved.history.document.runs.last.id
    assertEquals(
      reserved.effects,
      Vector(Effect.RequestRun(run, analysis, dataset), Effect.Persist)
    )
    assertEquals(reserved.history.document.run(run).map(_.analysis), Some(analysis))
    assert(reserved.history.undo.isLeft)
  }

  test("spatial commands reject a timed parent or family and duplicate restoration") {
    val timedParent = append match
      case Command.AppendLocationDataset(_, s, p, i) =>
        Command.AppendLocationDataset(Some(DatasetRevision(1)), s, p, i)
      case _ => fail("unexpected command")
    assert(Reducer.run(base, timedParent).isLeft)
    assert(Reducer.run(base, save(DatasetRevision(1))).isLeft)
    val appended = get(Reducer.run(base, append)).document
    val revision =
      appended.datasetDefinition(nextDataset).get.asInstanceOf[LocationDatasetRevision]
    assertEquals(
      Reducer.run(appended, Command.RestoreLocationDataset(revision)),
      Left(CommandError.DatasetExists(revision.id))
    )
  }

  test("discard inverses restore the exact legacy and gapped family registries") {
    val appended = get(History.start(base).apply(append)).history
    val saved    = get(appended.apply(save(nextDataset))).history
    // Undoing Save must recover the original legacy `None`, rather than
    // synthesizing an ownership registry for timed revisions.
    assertEquals(get(saved.undo).history.document, appended.document)

    val original = saved.document
    val location =
      original.analysisDefinition(nextAnalysis).get.asInstanceOf[LocationAnalysisRevision]
    val timed    = original.timedAnalyses.head
    val later    = timed.copy(id = AnalysisRevision(12))
    val legacy   = get(AnalysisFamily.of(AnalysisFamilyId.Legacy, timed.revisionName.value))
    val spatial  = get(AnalysisFamily.of(get(AnalysisFamilyId.of(9)), "gapped spatial family"))
    val gapped   = location.copy(id = AnalysisRevision(9))
    val families = get(
      AnalysisFamilyRegistry.of(
        Vector(legacy, spatial),
        Vector(
          get(AnalysisFamilyOwner.of(timed.id, legacy.id)),
          get(AnalysisFamilyOwner.of(gapped.id, spatial.id)),
          get(AnalysisFamilyOwner.of(later.id, legacy.id))
        ),
        Vector(timed.id, gapped.id, later.id)
      )
    )
    val gappedDocument = get(
      StudioDocument.of(
        original.datasets,
        Vector(timed, gapped, later),
        original.draft,
        original.runs,
        original.reporting,
        original.figures,
        original.presentation,
        original.jobs,
        Some(families)
      )
    )
    val discarded =
      get(Reducer.run(gappedDocument, Command.DiscardLocationAnalysis(gapped.id))).document
    val removed = get(
      Reducer.run(
        gappedDocument,
        Command.RemoveLocationAnalysis(gapped.id, Some(families), discarded.analysisFamilies)
      )
    ).document
    assertEquals(removed, discarded)
    assert(
      Reducer
        .run(
          gappedDocument,
          Command.RemoveLocationAnalysis(gapped.id, None, discarded.analysisFamilies)
        )
        .isLeft
    )
    val restored = get(
      Reducer.run(
        discarded,
        Command.RestoreLocationAnalysis(gapped, Some(families), discarded.analysisFamilies)
      )
    ).document
    assertEquals(restored, gappedDocument)
  }

  test("spatial workflow journal round-trips at version 11 and version 10 refuses it") {
    val entries = Vector(JournalEntry.Apply(append), JournalEntry.Apply(save(nextDataset)))
    val text    = get(CommandJournal.write(base, entries, checkpointEvery = 1))
    val replay  = get(CommandJournal.replay(base, text))
    assertEquals(replay.entries, entries)
    assertEquals(
      replay.history.document.analysisDefinition(nextAnalysis).map(_.kind),
      Some(StudyKind.Locations)
    )
    val ladder = get(CommandJournal.ladder)
    val line   = JournalLine.Entry(1, entries.last)
    assertEquals(
      get(io.circe.parser.parse(get(CommandJournal.encode(line)))).hcursor
        .downField("schema")
        .downField("version")
        .as[Int],
      Right(11)
    )
    val through10 = get(ladder.upTo(ladder.versions.dropRight(1).last))
    assert(through10.codec.encode(line).isLeft)
    assert(through10.codec.parse(get(CommandJournal.encode(line))).isLeft)
  }
