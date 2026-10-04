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

package eyes4s.studio.viz.plot

import eyes4s.plan.{MapPlacement, OffWindowPolicy}
import eyes4s.studio.app.tokens.{StageVariant, Theme}
import eyes4s.studio.core.assets.{AssetLink, Display}
import eyes4s.studio.core.selection.{FixationIndex, StudioRef}
import eyes4s.studio.viz.trial.{
  MarkStyle,
  StimulusRaster,
  TrialFixation,
  TrialRole,
  TrialSamples,
  TrialScene,
  TrialSceneInput
}
import intaglio.interaction.NamedPicking
import intaglio.value
import munit.ScalaCheckSuite
import org.scalacheck.{Gen, Prop}

/** Semantic bindings and scene summaries (ticket S4.6), over generated
  * scenes of every plot builder (the scale ladder, participant plot, scale
  * profile and timeline) and of the trial scene:
  *
  *  - every named grob of the scene is a mark, and resolves to the
  *    scientific identity it shows (its [[StudioRef]]s); every mark is drawn;
  *  - picking at a mark returns that mark's refs, never a position;
  *  - the scene carries its semantics: the plot's title, its description as
  *    alt text, and a text summary of what every mark accounts for.
  */
class SemanticBindingSuite extends ScalaCheckSuite:

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(60)

  private def right[E, A](e: Either[E, A]): A =
    e.fold(x => fail(s"unexpected Left: $x"), identity)

  private val genBuilt: Gen[BuiltPlot] =
    val ladder = for
      l <- LadderSamples.genLadder
      b <- LadderSamples.genBuilder(l)
    yield (LadderSamples.source(l), b: PlotBuilder)
    val participants = ParticipantSamples.genMeans.map(m =>
      (ParticipantSamples.source(m), ParticipantPlot(ParticipantSamples.columns): PlotBuilder)
    )
    val profiles = ProfileSamples.genProfile.map(p =>
      (ProfileSamples.source(p), ScaleProfilePlot(ProfileSamples.columns): PlotBuilder)
    )
    val timelines = TimelineSamples.genTimeline.map(t =>
      (TimelineSamples.source(t), TimelinePlot(TimelineSamples.columns): PlotBuilder)
    )
    for
      (source, builder) <- Gen.oneOf(ladder, participants, profiles, timelines)
      theme             <- Gen.oneOf(Theme.values.toSeq)
    yield right(builder.build(source, theme))

  property("every named grob of a plot is a mark, and resolves to its StudioRefs") {
    Prop.forAll(genBuilt) { plot =>
      val named = SceneSummaries.namedGrobs(plot.plot.scene)
      named.foreach(n =>
        assert(plot.refsNamed(n).exists(_.nonEmpty), s"${n.value} resolves to no StudioRef")
      )
      // Every mark is drawn as its named grob, and its rows are the source's.
      assertEquals(plot.marks.map(_.name).toSet, named.toSet)
      plot.marks.foreach(m =>
        m.refs.foreach(r => assertEquals(plot.markOf(r).map(_.name), Some(m.name)))
      )
      val sourceRefs = plot.source.rows.map(_.ref).toSet
      assert(plot.marks.flatMap(_.refs).forall(sourceRefs), "a mark names a row it lacks")
    }
  }

  property("picking a mark returns its scientific identity") {
    Prop.forAll(genBuilt, Gen.oneOf(1.0, 2.0)) { (plot, scale) =>
      val transform =
        right(PlotTransform.resolve(plot.plot, right(PlotSurface(640, 400, scale))))
      val picking = right(NamedPicking.compile(plot.plot.scene, transform.renderContext))
      val targets = right(PlotTargets.resolve(plot, transform, picking))
      plot.marks.foreach { m =>
        targets.target(m.ref).foreach { t =>
          // A pick names a mark of the plot, whose refs are its identity.
          right(targets.pick(t.anchor, 0.5 * scale)).foreach(hit =>
            assert(plot.markOf(hit.ref).exists(_.refs.contains(hit.ref)), hit.ref.toString)
          )
        }
      }
    }
  }

  property("the scene carries the plot's title, alt text and text summary") {
    Prop.forAll(genBuilt) { plot =>
      val semantics = plot.plot.scene.semantics
      assertEquals(semantics.plots.size, 1)
      val s = semantics.plots.head
      assertEquals(s.id, SceneSummaries.semanticId(plot.plot.id))
      assertEquals(semantics.documentId, Some(s.id))
      assertEquals(s.title, Some(plot.title))
      assertEquals(s.altText, Some(plot.description))
      assertEquals(s.accessibleDescription, plot.description)
      assertEquals(s.description, Some(plot.textSummary))
      val drawn = plot.source.rows.size - plot.unplotted.size
      assert(
        plot.textSummary.startsWith(
          s"${plot.title}: ${plot.marks.size} marks for $drawn rows."
        ),
        plot.textSummary
      )
      if plot.unplotted.isEmpty then assert(plot.textSummary.endsWith("Every row is drawn."))
      else
        plot.unplotted
          .flatMap(plot.unplottedText)
          .foreach(t => assert(plot.textSummary.contains(t), t))
    }
  }

  test("a semantic id keeps only Intaglio's portable characters") {
    assertEquals(
      SceneSummaries.semanticId(right(SceneId("scale ladder · P17/ret_07 σ2°"))).value,
      "studio-scale-ladder---P17-ret_07--2-"
    )
  }

  // --- the trial scene ------------------------------------------------------------------------

  private val genFixations: Gen[Vector[TrialFixation]] =
    for
      n  <- Gen.choose(1, 24)
      xs <- Gen.listOfN(n, Gen.choose(0.0, 1919.0))
      ys <- Gen.listOfN(n, Gen.choose(0.0, 1079.0))
      ds <- Gen.listOfN(n, Gen.choose(2, 1600))
      os <- Gen.listOfN(
        n,
        Gen.oneOf(
          MapPlacement.InMap,
          MapPlacement.OutsideWindow(OffWindowPolicy.Exclude),
          MapPlacement.OutsideScreen,
          MapPlacement.DroppedInitial
        )
      )
    yield xs.lazyZip(ys).lazyZip(ds).lazyZip(os).toVector.zipWithIndex.map {
      case ((x, y, d, o), i) =>
        right(TrialFixation.of(TrialSamples.ret07, right(FixationIndex.of(i + 1)), x, y, d, o))
    }

  private def trialScene(fs: Vector[TrialFixation], role: MarkStyle, theme: Theme): TrialScene =
    right(
      TrialScene(
        TrialSceneInput(
          TrialSamples
            .display(TrialSamples.ret07, Display.Image(AssetLink.Present(TrialSamples.beach))),
          TrialSamples.screen,
          fs,
          role,
          theme,
          StageVariant.Dark,
          Map(TrialSamples.beach -> StimulusRaster.Loaded(TrialSamples.raster))
        )
      )
    )

  property(
    "every fixation mark of a trial scene resolves to its fixation; the scene is summarised"
  ) {
    Prop.forAll(
      genFixations,
      Gen.oneOf(MarkStyle.Neutral +: TrialRole.values.toList.map(MarkStyle.Role(_))),
      Gen.oneOf(Theme.values.toSeq)
    ) { (fs, role, theme) =>
      val scene = trialScene(fs, role, theme)
      val marks = SceneSummaries
        .namedGrobs(scene.plot.scene)
        .filter(_.value.startsWith(TrialScene.MarkPrefix))
      marks.foreach(n =>
        assert(scene.refOf(n).isDefined, s"${n.value} resolves to no fixation")
      )
      assertEquals(
        marks.flatMap(scene.refOf).toSet,
        scene.marks.map(_.ref).toSet
      )
      scene.marks.foreach(m => assert(m.ref.isInstanceOf[StudioRef.Fixation]))
      val s = scene.plot.scene.semantics.plots.head
      assertEquals(s.altText, Some(scene.caption))
      assertEquals(s.title, Some(TrialSamples.ret07.label))
      assert(
        s.description.exists(_.contains(s"${scene.marks.size} fixation marks")),
        s.description.toString
      )
    }
  }
