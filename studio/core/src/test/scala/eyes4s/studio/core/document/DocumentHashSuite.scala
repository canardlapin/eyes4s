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
import eyes4s.plan.AdmissionDecision as CoreAdmissionDecision
import eyes4s.studio.core.backend.{AnalysisRevision, JobId, RunId}
import org.scalacheck.Prop.forAll

/** The document's scientific identity (ticket S2.1): the CR3 digest of its
  * science. No presentation change moves it; every science change does.
  */
class DocumentHashSuite extends munit.ScalaCheckSuite:
  import DocumentGen.*
  import DocumentSamples.{t1, t2, t3}

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(50)

  private def hash(d: StudioDocument): String =
    StudioDocument.scienceDigest(d).fold(e => fail(e.message), _.display)

  property("a presentation change never changes the scientific hash") {
    forAll(document) { (d: StudioDocument) =>
      forAll(presentation(d.runs.map(_.id))) { (p: PresentationState) =>
        assertEquals(d.withPresentation(p).map(hash), Right(hash(d)))
      }
    }
  }

  property("no presentation of a story moment changes its hash") {
    forAll(presentation(t3.runs.map(_.id))) { (p: PresentationState) =>
      assertEquals(t3.withPresentation(p).map(hash), Right(hash(t3)))
      val earlier = PresentationState.of(
        p.perspective,
        p.theme,
        p.stage,
        p.mapOpacity,
        p.underlay,
        p.shownRun.filter(_.number <= 7),
        p.layouts
      )
      assertEquals(earlier.flatMap(t2.withPresentation).map(hash), Right(hash(t2)))
    }
  }

  property("the hash survives a round trip through the stored document") {
    forAll(document) { (d: StudioDocument) =>
      assertEquals(
        StudioDocument.encode(d).flatMap(StudioDocument.decode).map(hash),
        Right(hash(d))
      )
    }
  }

  test("the story moments' hashes are pinned and distinct") {
    val actual = Vector("t1" -> hash(t1), "t2" -> hash(t2), "t3" -> hash(t3))
    actual.foreach((n, h) =>
      if !DocumentPins.science.get(n).contains(h) then println(s"PIN\t$n\t$h")
    )
    assertEquals(actual.toMap, DocumentPins.science)
    assertEquals(actual.map(_._2).distinct.size, 3)
    assert(hash(t1).startsWith("sha256:"))
  }

  /** Rebuild t2 with one science change. */
  private def rebuild(
      datasets: Vector[DatasetRevisionSpec] => Vector[DatasetRevisionSpec] = identity,
      analyses: Vector[AnalysisRevisionSpec] => Vector[AnalysisRevisionSpec] = identity,
      draft: Option[Draft] => Option[Draft] = identity,
      runs: Vector[RunRef] => Vector[RunRef] = identity,
      reporting: Vector[ReportingSpec] => Vector[ReportingSpec] = identity,
      figures: Vector[FigureSpec] => Vector[FigureSpec] = identity
  ): StudioDocument =
    right(
      StudioDocument.of(
        datasets(t2.datasets),
        analyses(t2.analyses),
        draft(t2.draft),
        runs(t2.runs),
        reporting(t2.reporting),
        figures(t2.figures),
        t2.presentation,
        t2.jobs
      )
    )

  private def r3(f: DatasetRevisionSpec => DatasetRevisionSpec) =
    (ds: Vector[DatasetRevisionSpec]) => ds.updated(1, f(ds(1)))
  private def rev4(f: AnalysisRevisionSpec => AnalysisRevisionSpec) =
    (as: Vector[AnalysisRevisionSpec]) => as.updated(1, f(as(1)))
  private def run7(f: RunRef => RunRef) = (rs: Vector[RunRef]) => rs.updated(2, f(rs(2)))
  private def fixations(f: Source => Source)(d: DatasetRevisionSpec) =
    d.copy(sources = right(Sources.of(d.sources.entries.updated(0, f(d.sources.entries(0))))))
  private def bound[A] = CoreBinding.Bound(CanonicalDigest.parse[A]("ab" * 32).toOption.get)
  private val spec     = t2.reporting(0)
  private def reported(
      minimum: Option[MinimumPerGroup] = spec.minimumPerGroup,
      weighting: ReportingWeight = spec.weighting,
      filters: Vector[ReportingFilter] = spec.filters
  ) = Vector(
    right(ReportingSpec.of(spec.id, spec.name, spec.groupBy, filters, minimum, weighting))
  )
  private val figure1 = t2.figures(0)
  private val figure2 = t2.figures(1)

  private val mutations: Vector[(String, StudioDocument)] = Vector(
    "source bytes" -> rebuild(datasets =
      r3(fixations(_.copy(bytes = ByteDigest.parse("cd" * 32).toOption.get)))
    ),
    "source semantic identity" -> rebuild(datasets =
      r3(fixations(_.copy(semantic = Some(right(SemanticIdentity.of("00112233445566ff"))))))
    ),
    "source path" -> rebuild(datasets =
      r3(fixations(_.copy(path = right(SourcePath.of("inputs/fixations-v2.csv")))))
    ),
    "column mapping" -> rebuild(datasets =
      r3(d =>
        d.copy(mapping =
          right(ColumnMapping.of(d.mapping.bindings.filterNot(_.role == ColumnRole.Occurrence)))
        )
      )
    ),
    "declared units"    -> rebuild(datasets = r3(_.copy(units = DeclaredUnits(None)))),
    "pixels per degree" -> rebuild(datasets =
      r3(d =>
        d.copy(geometry =
          right(
            Geometry
              .of(d.geometry.screen, d.geometry.image, right(DeclaredPixelsPerDegree.of(36.0)))
          )
        )
      )
    ),
    "image placement" -> rebuild(datasets =
      r3(d =>
        d.copy(geometry =
          right(
            Geometry.of(
              d.geometry.screen,
              right(ImagePlacement.of(447, 156, 1024, 768)),
              d.geometry.pixelsPerDegree
            )
          )
        )
      )
    ),
    "off-screen policy" -> rebuild(datasets =
      r3(d => d.copy(admission = d.admission.copy(offScreen = OffScreenChoice.QuarantineTrial)))
    ),
    "corrections" -> rebuild(datasets =
      r3(d =>
        d.copy(admission =
          d.admission.copy(corrections =
            Vector(CorrectionRule(CorrectionTarget.AllTrials, CoordinateCorrection.FlipY))
          )
        )
      )
    ),
    "admission decision" -> rebuild(datasets =
      r3(_.copy(decision = AdmissionDecision.Pending))
    ),
    "ledger binding" -> rebuild(datasets =
      r3(d =>
        d.copy(decision =
          AdmissionDecision.Admitted(d.decision.admittedUnder, bound, CoreBinding.unbound)
        )
      )
    ),
    "admission policy" -> rebuild(datasets =
      r3(
        _.copy(decision =
          AdmissionDecision.Admitted(
            Some(CoreAdmissionDecision.RequireComplete),
            CoreBinding.unbound,
            CoreBinding.unbound
          )
        )
      )
    ),
    "dataset parent" -> rebuild(datasets = r3(_.copy(parent = None))),
    "plan binding"   -> rebuild(analyses = rev4(_.copy(plan = bound))),
    "recipe grid"    -> rebuild(
      analyses = rev4(a => a.copy(recipe = a.recipe.copy(grid = right(GridSize.of(32, 24))))),
      draft = _ => None
    ),
    "analysis name" -> rebuild(analyses =
      rev4(a => a.copy(studio = a.studio.copy(name = right(RevisionName.of("Other")))))
    ),
    "analysis description" -> rebuild(analyses =
      rev4(a => a.copy(studio = a.studio.copy(description = "methods note")))
    ),
    "analysis preset" -> rebuild(analyses =
      rev4(a => a.copy(studio = a.studio.copy(preset = Preset.Custom)))
    ),
    "draft discarded" -> rebuild(draft = _ => None),
    "draft change"    -> rebuild(draft =
      _ =>
        Some(
          right(
            Draft.between(
              AnalysisRevision(5),
              t2.analyses(1),
              t2.analyses(1).recipe.copy(unmatched = UnmatchedChoice.Refuse)
            )
          )
        )
    ),
    "run state"           -> rebuild(runs = run7(_.copy(state = RunLifecycle.Failed))),
    "run archive binding" -> rebuild(runs = run7(_.copy(archive = bound))),
    "run removed"         -> rebuild(runs = rs => rs.patch(1, Nil, 1)),
    "minimum per group"   -> rebuild(reporting =
      _ => reported(minimum = Some(right(MinimumPerGroup.of(3))))
    ),
    "weighting" -> rebuild(reporting =
      _ => reported(weighting = ReportingWeight.PooledQueries)
    ),
    "reporting filter" -> rebuild(reporting =
      _ => reported(filters = Vector(ReportingFilter.OutsideWindowAtMost(right(Share.of(0.5)))))
    ),
    "panel scale" -> rebuild(figures =
      _ =>
        Vector(
          right(
            FigureSpec.of(
              figure1.id,
              figure1.run,
              figure1.reporting,
              figure1.panels
                .updated(3, figure1.panels(3).copy(scale = PanelScale.At(right(Sigma.of(4.0)))))
            )
          ),
          figure2
        )
    ),
    "figure rebound" -> rebuild(figures =
      _ =>
        Vector(
          figure1,
          right(FigureSpec.of(figure2.id, RunId(7), figure2.reporting, figure2.panels))
        )
    )
  )

  test("every science change changes the hash, and no two changes collide") {
    val base    = hash(t2)
    val changed = mutations.map((name, d) => name -> hash(d))
    assertEquals(changed.filter(_._2 == base).map(_._1), Vector.empty)
    assertEquals(changed.map(_._2).distinct.size, changed.size)
  }

  property("job handles never change the scientific hash") {
    forAll(document) { (d: StudioDocument) =>
      forAll(jobs(d.runs)) { (js: Vector[JobHandle]) =>
        assertEquals(d.withJobs(js).map(hash), Right(hash(d)))
      }
    }
  }

  test("t3's hash is the same without run 8's job handle") {
    assertEquals(t3.withJobs(Vector.empty).map(hash), Right(hash(t3)))
    assertEquals(t3.withJobs(Vector(JobHandle(RunId(8), JobId(42)))).map(hash), Right(hash(t3)))
  }

  test("the order of reporting filters does not change the hash") {
    val keep = ReportingFilter.Keep(
      right(Covariate.of("response")),
      right(ValueSet.of(right(Covariate.of("response")), Vector("Remembered")))
    )
    val out = ReportingFilter.OutsideWindowAtMost(right(Share.of(0.5)))
    assertEquals(
      hash(rebuild(reporting = _ => reported(filters = Vector(out, keep)))),
      hash(rebuild(reporting = _ => reported(filters = Vector(keep, out, keep))))
    )
  }
