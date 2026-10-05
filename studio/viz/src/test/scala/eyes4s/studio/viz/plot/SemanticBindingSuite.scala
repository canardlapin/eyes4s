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
    // The edge cases, forced: no data at all, every participant mean
    // missing, and a ladder whose many controls have no cosine (undrawn).
    val edges: Gen[(eyes4s.studio.app.plot.PlotSource, PlotBuilder)] = Gen.oneOf(
      (
        TimelineSamples.source(TimelineSamples.timeline(Vector.empty)),
        TimelinePlot(TimelineSamples.columns): PlotBuilder
      ),
      (
        ParticipantSamples.source(ParticipantSamples.zeroAndMissing),
        ParticipantPlot(ParticipantSamples.columns): PlotBuilder
      ),
      (
        ParticipantSamples.source(
          ParticipantSamples.means(
            Vector(("Remembered", 0.1, 0)),
            Vector("P01" -> Vector(None), "P02" -> Vector(None))
          )
        ),
        ParticipantPlot(ParticipantSamples.columns): PlotBuilder
      ),
      (
        ProfileSamples.source(
          ProfileSamples.profile(ProfileSamples.protocol, Vector.empty, Vector.empty)
        ),
        ScaleProfilePlot(ProfileSamples.columns): PlotBuilder
      ),
      (
        LadderSamples.source(manyUndrawn),
        ScaleLadderPlot(LadderSamples.columns, None): PlotBuilder
      )
    )
    for
      (source, builder) <- Gen.frequency(
        3 -> ladder,
        3 -> participants,
        3 -> profiles,
        3 -> timelines,
        2 -> edges
      )
      theme <- Gen.oneOf(Theme.values.toSeq)
    yield right(builder.build(source, theme))

  /** One scale with 300 controls, none of them with a served cosine. */
  private lazy val manyUndrawn: eyes4s.studio.app.plot.ScaleLadder =
    LadderSamples.ladder(LadderSamples.Spec("2°", 0.73, 0.35, 0.38, Vector.fill(300)(None)))

  property("every named grob of a plot is a mark, and resolves to its StudioRefs") {
    Prop.forAll(genBuilt) { plot =>
      val named = SceneSummaries.namedGrobs(plot.plot.scene)
      // Each mark is drawn once: no name is borne by two grobs.
      assertEquals(SceneSummaries.duplicateNames(plot.plot.scene), Vector.empty)
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
    Prop.forAll(genBuilt) { plot =>
      List(1.0, 2.0).foreach { scale =>
        val transform =
          right(PlotTransform.resolve(plot.plot, right(PlotSurface(640, 400, scale))))
        val picking = right(NamedPicking.compile(plot.plot.scene, transform.renderContext))
        val targets = right(PlotTargets.resolve(plot, transform, picking))
        plot.marks.foreach { m =>
          // Every drawn mark is a target, and a pick at its anchor hits a mark:
          // this one, or one drawn over the same point (marks may overlap).
          val t   = targets.target(m.ref).getOrElse(fail(s"mark ${m.ref} is no target"))
          val hit = right(targets.pick(t.anchor, 0.5 * scale))
            .getOrElse(fail(s"a pick at the anchor of ${m.ref} hits nothing"))
          // A mark can be a line: its roving anchor is its first placed point,
          // while its painted geometry spans every placed point. Named picking
          // is therefore the exact authority for an overlapping mark; the
          // circular roving reach is only the fallback when no named geometry
          // was hit.
          val named = right(picking.hits(t.anchor, 0.5 * scale)).iterator
            .flatMap(h => targets.targets.find(_.mark.name == h.name))
            .nextOption()
          named match
            case Some(exact) => assertEquals(hit.mark, exact.mark)
            case None        =>
              val fallback = targets.targets.reverseIterator.find { other =>
                math.hypot(other.anchor.x - t.anchor.x, other.anchor.y - t.anchor.y) <=
                  other.reachPx * scale
              }
              assertEquals(Some(hit), fallback)
          // The pick is scientific identity: the refs of the mark it names.
          assertEquals(plot.markOf(hit.ref).map(_.refs), Some(hit.mark.refs))
        }
      }
    }
  }

  test(
    "a later profile line wins a named pick at its middle vertex, despite its remote roving anchor"
  ) {
    // Reduced fixed counterexample from hosted seed KF-5ODW0Oql7V6UtJPuIz4AYg0y5L9UT29PCS0rUhuM=.
    val profile = ProfileSamples.profile(
      Vector(0.5, 2.0, 8.0),
      Vector(
        ("Remembered", 24, Vector(Some(0.46), Some(0.28), Some(0.39))),
        ("Forgotten", 24, Vector(Some(0.45), Some(-0.50), Some(0.91)))
      ),
      Vector(
        ("P01", 10, Vector(Some(-0.34), Some(0.83), Some(0.75))),
        ("P02", 10, Vector(None, Some(0.5782826402376102), Some(-0.41))),
        ("P03", 10, Vector(Some(-0.29), Some(0.35), None)),
        ("P04", 10, Vector(Some(-0.9881085085399421), Some(0.5776091106507737), Some(0.67)))
      )
    )
    val plot = right(
      ScaleProfilePlot(ProfileSamples.columns)
        .build(ProfileSamples.source(profile), Theme.Light)
    )
    List(1.0, 2.0).foreach { scale =>
      val transform =
        right(PlotTransform.resolve(plot.plot, right(PlotSurface(640, 400, scale))))
      val picking = right(NamedPicking.compile(plot.plot.scene, transform.renderContext))
      val targets = right(PlotTargets.resolve(plot, transform, picking))
      val p02 = targets.target(profile.participants(1).points.head.ref).getOrElse(fail("P02"))
      val p04 = targets.target(profile.participants(3).points.head.ref).getOrElse(fail("P04"))
      val distance = math.hypot(p04.anchor.x - p02.anchor.x, p04.anchor.y - p02.anchor.y)

      // P04's middle vertex is under P02's anchor within the device-pixel query,
      // although P04's first-point roving anchor is not.
      assert(distance > (p04.reachPx + 0.5) * scale + 1e-6, distance)
      assertEquals(
        right(picking.hits(p02.anchor, 0.5 * scale)).headOption.map(_.name),
        Some(p04.mark.name)
      )
      assertEquals(right(targets.pick(p02.anchor, 0.5 * scale)).map(_.ref), Some(p04.ref))
      assertEquals(plot.markOf(p04.ref).map(_.refs), Some(p04.mark.refs))
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
        assert(
          plot.textSummary.contains(s"${plot.unplotted.size} rows not drawn: "),
          plot.textSummary
        )
      // Bounded whatever the number of rows: three labels at most, then a count.
      assert(plot.textSummary.length <= plot.title.length + 400, plot.textSummary)
    }
  }

  test("many undrawn rows: counted, grouped by reason, three named, and how many more") {
    val plot = right(
      ScaleLadderPlot(LadderSamples.columns, None)
        .build(LadderSamples.source(manyUndrawn), Theme.Light)
    )
    assertEquals(plot.unplotted.size, 300)
    val summary = plot.textSummary
    assert(summary.contains("300 rows not drawn: no "), summary)
    assert(summary.contains("(300)"), summary)
    assert(summary.endsWith("and 297 more."), summary)
    assert(summary.length < 400, s"${summary.length}: $summary")
  }

  test("a name two grobs bear is a duplicate: a mark is drawn once") {
    val name  = right(intaglio.GraphicsName("dot-0", "test"))
    val twice = intaglio.Scene(
      Vector(
        intaglio.Grob.group(Vector.empty, name = Some(name)),
        intaglio.Grob.group(Vector.empty, name = Some(name))
      )
    )
    assertEquals(SceneSummaries.duplicateNames(twice), Vector(name))
    assertEquals(SceneSummaries.namedGrobs(twice), Vector(name, name))
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
          MapPlacement.InWindow,
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
      Gen.frequency(5 -> genFixations, 1 -> Gen.const(Vector.empty[TrialFixation])),
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
