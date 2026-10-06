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

import eyes4s.codec.CanonicalDigest
import eyes4s.studio.core.artifacts.NativeBindingFacts
import eyes4s.studio.core.backend.{AnalysisRevision, DatasetRevision, RunId}
import eyes4s.studio.core.document.*
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

class BindCompletedArtifactsSuite extends munit.ScalaCheckSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val base                              = DocumentSamples.t2
  private val facts                             = CommandSamples.nativeFacts
  private def mutate(
      run: RunId = facts.run,
      revision: AnalysisRevision = facts.revision,
      dataset: DatasetRevision = facts.dataset,
      source: SemanticIdentity = facts.source,
      result: CanonicalDigest[ResultArchiveArtifact] = facts.result,
      recipe: Recipe = facts.recipeSnapshot
  ): NativeBindingFacts =
    get(
      NativeBindingFacts.of(
        run,
        facts.stamp.copy(revision = revision, dataset = dataset),
        source,
        result,
        recipe
      )
    )
  private def document(
      analyses: Vector[AnalysisRevisionSpec] = base.analyses,
      runs: Vector[RunRef] = base.runs,
      datasets: Vector[DatasetRevisionSpec] = base.datasets
  ): StudioDocument = get(
    StudioDocument.of(
      datasets,
      analyses,
      base.draft,
      runs,
      base.reporting,
      base.figures,
      base.presentation,
      base.jobs
    )
  )

  test(
    "native completion bindings are one checked fact, leave view/history intact, and replay identically"
  ) {
    val history  = History.start(base)
    val entry    = JournalEntry.Apply(Command.BindCompletedArtifacts(facts))
    val bound    = get(history.perform(entry))
    val analysis = bound.history.document.analysis(facts.revision).get
    val run      = bound.history.document.run(facts.run).get
    assertEquals(analysis.plan, facts.stamp.plan)
    assertEquals(analysis.recipe.input, Some(facts.source))
    assertEquals(run.archive, CoreBinding.Bound(facts.result))
    assertEquals(bound.history.document.presentation, base.presentation)
    assertEquals(bound.history.science, history.science)
    assertEquals(bound.history.presentation, history.presentation)
    assertEquals(bound.effects, Vector(Effect.Persist))
    val again = get(bound.history.perform(entry))
    assertEquals(again.history, bound.history)
    assertEquals(again.effects, Vector.empty)
    val text   = get(CommandJournal.write(base, Vector(entry, entry), checkpointEvery = 1))
    val replay = get(CommandJournal.replay(base, text))
    assertEquals(replay.history.document, bound.history.document)
    assertEquals(replay.entries, Vector(entry, entry))
  }

  test("unknown, unfinished, wrong-scope and different-science facts refuse atomically") {
    val unfinished = document(runs =
      base.runs.map(r => if r.id == facts.run then r.copy(state = RunLifecycle.Running) else r)
    )
    assertEquals(
      Reducer.run(unfinished, Command.BindCompletedArtifacts(facts)),
      Left(CommandError.RunNotCompleted(facts.run, RunLifecycle.Running))
    )
    assertEquals(
      Reducer.run(base, Command.BindCompletedArtifacts(mutate(run = RunId(99)))),
      Left(CommandError.UnknownRun(RunId(99)))
    )
    Vector(mutate(revision = AnalysisRevision(5)), mutate(dataset = DatasetRevision(2)))
      .foreach { wrong =>
        assert(
          Reducer
            .run(base, Command.BindCompletedArtifacts(wrong))
            .left
            .toOption
            .exists(_.isInstanceOf[CommandError.ArtifactScopeMismatch])
        )
      }
    val changed = mutate(recipe = facts.recipeSnapshot.copy(grid = get(GridSize.of(32, 24))))
    assert(
      Reducer
        .run(base, Command.BindCompletedArtifacts(changed))
        .left
        .toOption
        .exists(_.isInstanceOf[CommandError.ArtifactRecipeMismatch])
    )
    assertEquals(base.analysis(facts.revision).get.plan, CoreBinding.unbound[StudyPlanArtifact])
    assertEquals(base.run(facts.run).get.archive, CoreBinding.unbound[ResultArchiveArtifact])
  }

  test(
    "contradictory existing plan, semantic source and result bindings refuse without clearing anything"
  ) {
    val otherPlan = get(CanonicalDigest.parse[StudyPlanArtifact]("4" * 64))
    val wrongPlan = document(analyses =
      base.analyses.map(a =>
        if a.id == facts.revision then a.copy(plan = CoreBinding.Bound(otherPlan)) else a
      )
    )
    assert(
      Reducer
        .run(wrongPlan, Command.BindCompletedArtifacts(facts))
        .left
        .toOption
        .exists(_.isInstanceOf[CommandError.ArtifactBindingMismatch])
    )
    val otherSource = get(SemanticIdentity.of("ffeeddccbbaa9988"))
    val wrongSource = document(analyses =
      base.analyses.map(a =>
        if a.id == facts.revision then a.copy(recipe = a.recipe.copy(input = Some(otherSource)))
        else a
      )
    )
    assertEquals(
      Reducer.run(wrongSource, Command.BindCompletedArtifacts(facts)),
      Left(CommandError.InputMismatch(facts.revision, otherSource, facts.source))
    )
    val otherResult = get(CanonicalDigest.parse[ResultArchiveArtifact]("5" * 64))
    val wrongResult = document(runs =
      base.runs.map(r =>
        if r.id == facts.run then r.copy(archive = CoreBinding.Bound(otherResult)) else r
      )
    )
    assert(
      Reducer
        .run(wrongResult, Command.BindCompletedArtifacts(facts))
        .left
        .toOption
        .exists(_.isInstanceOf[CommandError.ArtifactBindingMismatch])
    )
  }

  test("declared fixation source identity must match verified source before any binding") {
    def declared(source: SemanticIdentity): StudioDocument = document(datasets =
      base.datasets.map(dataset =>
        if dataset.id == facts.dataset then
          dataset.copy(sources =
            get(
              Sources.of(
                dataset.sources.entries.map(entry =>
                  if entry.role == SourceRole.Fixations then entry.copy(semantic = Some(source))
                  else entry
                )
              )
            )
          )
        else dataset
      )
    )
    val matching =
      get(Reducer.run(declared(facts.source), Command.BindCompletedArtifacts(facts)))
    assertEquals(matching.document.run(facts.run).get.archive, CoreBinding.Bound(facts.result))
    val other       = get(SemanticIdentity.of("ffeeddccbbaa9988"))
    val mismatching = declared(other)
    assertEquals(
      Reducer.run(mismatching, Command.BindCompletedArtifacts(facts)),
      Left(
        CommandError.ArtifactBindingMismatch(
          facts.run,
          s"${facts.dataset.label} fixation source semantic",
          other.value,
          facts.source.value
        )
      )
    )
    assertEquals(
      mismatching.analysis(facts.revision).get.plan,
      base.analysis(facts.revision).get.plan
    )
    assertEquals(mismatching.run(facts.run).get.archive, base.run(facts.run).get.archive)
  }

  property(
    "every generated completed-run binding preserves presentation, binds all operands and is idempotent"
  ) {
    val completed = document(runs = base.runs.filter(_.state == RunLifecycle.Completed))
    val generated = Gen
      .oneOf(CommandGen.backend(completed))
      .flatMap(identity)
      .suchThat(_.isInstanceOf[Command.BindCompletedArtifacts])
    forAll(generated) {
      case command @ Command.BindCompletedArtifacts(produced) =>
        val outcome = get(Reducer.run(completed, command))
        assertEquals(outcome.document.presentation, completed.presentation)
        assertEquals(outcome.document.analysis(produced.revision).get.plan, produced.stamp.plan)
        assertEquals(
          outcome.document.analysis(produced.revision).get.recipe.input,
          Some(produced.source)
        )
        assertEquals(
          outcome.document.run(produced.run).get.archive,
          CoreBinding.Bound(produced.result)
        )
        assertEquals(
          Reducer.run(outcome.document, command).map(_.document),
          Right(outcome.document)
        )
      case other => fail(s"unexpected generated $other")
    }
  }

  test(
    "binding facts alone require journal5; every older writer/reader refuses without treating it as torn"
  ) {
    val entry  = JournalEntry.Apply(Command.BindCompletedArtifacts(facts))
    val line   = JournalLine.Entry(1, entry)
    val ladder = get(CommandJournal.ladder)
    val text   = get(CommandJournal.encode(line))
    assertEquals(
      get(io.circe.parser.parse(text)).hcursor.downField("schema").downField("version").as[Int],
      Right(5)
    )
    val through4 = get(ladder.upTo(ladder.versions(3)))
    assert(through4.codec.encode(line).isLeft)
    assert(through4.codec.parse(text).isLeft)
    val journal = get(CommandJournal.start(base)) + "\n" + text + "\n"
    assert(CommandJournal.replay(Right(through4.codec), base, journal).isLeft)
    (0 to 3).foreach { index =>
      assert(ladder.writeAt(ladder.versions(index), line).isLeft)
      val relabelled = get(io.circe.parser.parse(text)).mapObject(
        _.add(
          "schema",
          io.circe.Json.obj(
            "name"    -> io.circe.Json.fromString("studio.journal"),
            "version" -> io.circe.Json.fromInt(index + 1)
          )
        )
      )
      assert(
        ladder
          .readAt(ladder.versions(index), relabelled.hcursor.downField("value").focus.get)
          .isLeft
      )
    }
  }
