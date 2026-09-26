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

package eyes4s.laws

import eyes4s.compare.Similarity
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.{Deg, Px}
import eyes4s.plan.*
import eyes4s.surface.EdgePolicy
import org.scalacheck.{Gen, Test}

/** Runs the published initial-fixation and structural-diff laws against the
  * shipped implementations, and shows by mutation that each law discriminates.
  *
  * {{{
  * | law set               | killed mutants                                         |
  * |-----------------------|--------------------------------------------------------|
  * | initialFixations      | keep-all drops the first fixation, an open disc, every |
  * |                       | fixation in the disc dropped, a tally that miscounts   |
  * | initialFixationRoles  | the policy applied to focal trials only                |
  * | studyDiff             | a dropped field, a reversed order, a one-sided diff, a |
  * |                       | revision that ignores staleness or one field           |
  * }}}
  */
class StudyRevisionLawsSuite extends munit.DisciplineSuite:
  import StudyRevisionLawsSuite.*

  checkAll(
    "InitialFixationRule",
    InitialFixationLaws.selection(
      InitialFixationLaws.shipped[Px],
      InitialFixationLaws.crossCases
    )
  )
  checkAll(
    "StudyPlan initial fixations",
    InitialFixationLaws.roles[StudyKey, Px, Unit, Similarity, SignedDifference](
      (plan, input) => plan.initialFixationTallies(input),
      InitialFixationLaws.estimated,
      InitialFixationLaws.planCases
    )
  )
  private val (between, revise) =
    StudyDiffLaws.shipped[StudyKey, Px, Unit, Similarity, SignedDifference]
  checkAll("StudyDiff", StudyDiffLaws.diff(between, revise, pairs))

  // -------------------------------------------------------------------------
  // Mutants
  // -------------------------------------------------------------------------

  /** Mutant checks run from a fixed seed, so every kill is reproducible. */
  private val parameters =
    Test.Parameters.default.withMinSuccessfulTests(100).withInitialSeed(0x524556495345L)

  private def outcomes(rules: StudyRevisionLawsSuite.Laws): Vector[(String, Boolean)] =
    rules.all.properties.toVector.map((name, prop) =>
      name -> (Test.check(parameters, prop).status match
        case Test.Failed(_, _) => true
        case _                 => false)
    )

  /** The property names a mutant falsifies, without the rule-set prefix. */
  private def falsified(rules: StudyRevisionLawsSuite.Laws): Vector[String] =
    outcomes(rules).collect { case (name, true) => name.drop(name.indexOf('.') + 1) }.sorted

  private def passes(rules: StudyRevisionLawsSuite.Laws): Boolean =
    rules.all.properties.forall((_, prop) => Test.check(parameters, prop).passed)

  private def tally(path: Scanpath[Px], dropped: Int, total: Int): InitialFixationTally =
    val fixations = path.fixations.toVector
    InitialFixationTally
      .of(
        dropped,
        total,
        fixations.take(dropped).map(_.duration).foldLeft(Span.zero)(_ + _),
        path.dwellTotal
      )
      .fold(e => throw new AssertionError(e.message), identity)

  /** A selection that drops `count(rule, path)` leading fixations. */
  private def dropping(
      count: (InitialFixationRule[Px], Scanpath[Px]) => Int
  ): InitialFixationLaws.Selection[Px] = (rule, path) =>
    val n = count(rule, path)
    (tally(path, n, path.n), path.dropLeading(n))

  private def inDisc(rule: InitialFixationRule[Px], f: Event.Fixation[Px], open: Boolean) =
    rule.policy match
      case InitialFixationPolicy.DropLeadingInClosedDisc(cross, radius) =>
        val d = f.centre.distanceTo(cross) / rule.unitsPerDegree.get
        if open then d < radius else d <= radius
      case _ => false

  test("the shipped selection passes, and each selection mutant is killed") {
    val cases = InitialFixationLaws.crossCases
    assert(passes(InitialFixationLaws.selection(InitialFixationLaws.shipped[Px], cases)))
    val keepDrops = dropping((rule, path) =>
      if rule.policy.isKeepAll then math.min(1, path.n) else rule.dropCount(path)
    )
    assertEquals(
      falsified(InitialFixationLaws.selection(keepDrops, cases)),
      Vector(
        "keeping every fixation is the identity",
        "the dropped fixations are exactly those the policy names"
      )
    )
    val open = dropping((rule, path) =>
      rule.policy match
        case InitialFixationPolicy.DropLeadingInClosedDisc(_, _) =>
          path.fixations.toVector.takeWhile(inDisc(rule, _, open = true)).size
        case _ => rule.dropCount(path)
    )
    assertEquals(
      falsified(InitialFixationLaws.selection(open, cases)),
      Vector("the dropped fixations are exactly those the policy names")
    )
    val everyInside = dropping((rule, path) =>
      rule.policy match
        case InitialFixationPolicy.DropLeadingInClosedDisc(_, _) =>
          path.fixations.toVector.count(inDisc(rule, _, open = false))
        case _ => rule.dropCount(path)
    )
    assertEquals(
      falsified(InitialFixationLaws.selection(everyInside, cases)),
      Vector("the dropped fixations are exactly those the policy names")
    )
    val miscounted: InitialFixationLaws.Selection[Px] = (rule, path) =>
      val n = rule.dropCount(path)
      (tally(path, 0, path.n), path.dropLeading(n))
    assert(
      falsified(InitialFixationLaws.selection(miscounted, cases))
        .contains("dropped and kept fixations partition the trial, the kept ones its suffix")
    )
  }

  test("the roles law kills a policy applied to focal trials only, in tallies or maps") {
    val cases = InitialFixationLaws.planCases
    val focalOnly
        : InitialFixationLaws.Tallies[StudyKey, Px, Unit, Similarity, SignedDifference] =
      (plan, input) =>
        input.trials.rows.map { t =>
          val dropped =
            if plan.layout.phase(t.key) == plan.focalPhase then
              plan.initialFixationRule.dropCount(t.value)
            else 0
          t.key -> Right(tally(t.value, dropped, t.value.n))
        }
    // Maps of focal trials under the policy, and of reference trials as if
    // every fixation were kept.
    val focalMaps
        : InitialFixationLaws.Estimated[StudyKey, Px, Unit, Similarity, SignedDifference] =
      (plan, input) =>
        val shipped =
          InitialFixationLaws.estimated[StudyKey, Px, Unit, Similarity, SignedDifference]
        val keep = sure(
          plan.revise(
            Vector(
              StudyChange.InitialFixations(plan.initialFixations, InitialFixationPolicy.keepAll)
            )
          )
        )
        for
          under <- shipped(plan, input)
          all   <- shipped(keep, input)
        yield under.zip(all).map { case ((key, mapped), (_, whole)) =>
          key -> (if plan.layout.phase(key) == plan.focalPhase then mapped else whole)
        }
    val estimated =
      InitialFixationLaws.estimated[StudyKey, Px, Unit, Similarity, SignedDifference]
    assert(passes(InitialFixationLaws.roles(shippedTallies, estimated, cases)))
    assertEquals(
      falsified(InitialFixationLaws.roles(focalOnly, estimated, cases)),
      Vector(
        "each trial's tally is its own selection, whatever its phase",
        "exchanging the focal and reference phases leaves every tally unchanged"
      )
    )
    assertEquals(
      falsified(InitialFixationLaws.roles(shippedTallies, focalMaps, cases)),
      Vector("each trial is mapped from the fixations it keeps, whatever its phase")
    )
  }

  test(
    "the diff laws kill a dropped field, a reversed order, a one-sided diff and bad revisions"
  ) {
    assert(passes(StudyDiffLaws.diff(between, revise, pairs)))
    val noInitial: StudyDiffLaws.Between[StudyKey, Px, Unit, Similarity, SignedDifference] =
      (a, b) => between(a, b).filterNot(_.field == StudyField.InitialFixations)
    assertEquals(
      falsified(StudyDiffLaws.diff(noInitial, revise, pairs)),
      Vector(
        "a diff is empty exactly when the plans describe the same study",
        "applying the diff of a to b to a gives b"
      )
    )
    val reversed: StudyDiffLaws.Between[StudyKey, Px, Unit, Similarity, SignedDifference] =
      (a, b) => between(a, b).reverse
    assertEquals(
      falsified(StudyDiffLaws.diff(reversed, revise, pairs)),
      Vector("a diff has one change per field, in field order")
    )
    // A diff that reports a change only from the plan with the smaller input
    // digest, so it disagrees with itself in the other direction.
    val oneSided: StudyDiffLaws.Between[StudyKey, Px, Unit, Similarity, SignedDifference] =
      (a, b) =>
        if a.input.digest <= b.input.digest then between(a, b)
        else between(a, b).filterNot(_.field == StudyField.Scales)
    assert(
      falsified(StudyDiffLaws.diff(oneSided, revise, pairs))
        .contains("the diff is antisymmetric")
    )
    // A revision that applies every change without checking where it starts.
    val unchecked: StudyDiffLaws.Revise[StudyKey, Px, Unit, Similarity, SignedDifference] =
      (plan, changes) =>
        val rebased = changes.map(c => rebase(plan, c))
        plan.revise(rebased)
    assertEquals(
      falsified(StudyDiffLaws.diff(between, unchecked, pairs)),
      Vector("a change that does not start from the plan's value is refused")
    )
    // Diffs that never report a change of layout, or of method.
    Vector(StudyField.Layout, StudyField.Method).foreach { dropped =>
      val blind: StudyDiffLaws.Between[StudyKey, Px, Unit, Similarity, SignedDifference] =
        (a, b) => between(a, b).filterNot(_.field == dropped)
      assert(
        falsified(StudyDiffLaws.diff(blind, revise, pairs))
          .contains("applying the diff of a to b to a gives b"),
        dropped
      )
    }
    // A revision that leaves the weighting as it was.
    val forgetful: StudyDiffLaws.Revise[StudyKey, Px, Unit, Similarity, SignedDifference] =
      (plan, changes) => plan.revise(changes.filterNot(_.field == StudyField.Weighting))
    assert(
      falsified(StudyDiffLaws.diff(between, forgetful, pairs))
        .contains("applying the diff of a to b to a gives b")
    )
  }

  test("the default rendering names the field and the typed values") {
    val screen = sure(Frame.screen("render-screen", 1920, 1080))
    val image = sure(Subframe.centred(screen, FrameId("image"), sure(Extent.of[Px](1024, 768))))
    val base  = sure(
      StudyPlan.configure(
        sure(ArtifactRef.parse[StudyInput[StudyKey, Px]]("0123456789abcdef")),
        StudyKey.layout(DefinitionId.studyLayout),
        StudyGeometry.WholeFrame(sure(Grid.over(screen, 64, 36))),
        "recall",
        "encode",
        Weight.Duration,
        Vector(
          StudyScale.Angular[Px](
            StudyEstimate.Gaussian[Deg](sure(Sigma.deg(1.0)), EdgePolicy.Truncate)
          )
        ),
        Some(sure(LinearAngularScale.of(screen, 35.0))),
        FailurePolicy.RequireAll,
        StudyMethod.cosine[Px](DefinitionId.cosine),
        ()
      )
    )
    val eight =
      StudyScale.Angular[Px](
        StudyEstimate.Gaussian[Deg](sure(Sigma.deg(8.0)), EdgePolicy.Truncate)
      )
    val added = sure(base.revise(Vector(StudyChange.Scales(base.scales, base.scales :+ eight))))
    assertEquals(StudyDiff.render(base.structuralDiff(added)), "scales +8°")
    assertEquals(StudyDiff.render(added.structuralDiff(base)), "scales −8°")
    val mean = sure(
      base.revise(
        Vector(StudyChange.Matched(MatchedReferences.RequireOne, MatchedReferences.MeanOfAll))
      )
    )
    assertEquals(
      StudyDiff.render(base.structuralDiff(mean)),
      "matched references policy RequireOne → MeanOfAll"
    )
    val windowed = sure(
      base.revise(
        Vector(
          StudyChange.Grid(base.grid, sure(Grid.over(image.frame, 64, 48))),
          StudyChange.Window(None, Some(image)),
          StudyChange.OffWindow(None, Some(OffWindowPolicy.Exclude))
        )
      )
    )
    assertEquals(
      base.structuralDiff(windowed).map(_.field),
      Vector(StudyField.Grid, StudyField.Window, StudyField.OffWindow)
    )
    assertEquals(
      base.structuralDiff(windowed)(1).render,
      "window added: image [448, 1472) × [156, 924) px"
    )
    val cross   = sure(InitialFixationPolicy.dropLeadingInClosedDisc(Pt[Px](960, 540), 1.5))
    val dropped = sure(
      base.revise(Vector(StudyChange.InitialFixations(InitialFixationPolicy.keepAll, cross)))
    )
    assertEquals(
      StudyDiff.render(base.structuralDiff(dropped)),
      "initial fixations keep all → drop leading fixations within 1.5° of the cross"
    )
    assertEquals(StudyDiff.render(base.structuralDiff(base)), "no changes")
    // A change that does not start from the plan's value names both values.
    assertEquals(
      mean.revise(
        Vector(
          StudyChange.Matched(MatchedReferences.RequireOne, MatchedReferences.SameOccurrence)
        )
      ),
      Left(
        StudyRevisionError.Stale(
          StudyField.MatchedReferences,
          "RequireOne",
          "MeanOfAll"
        )
      )
    )
    assertEquals(
      base.revise(Vector(StudyChange.Window(None, Some(image)))),
      Left(StudyRevisionError.IncompleteWindow(Some("image"), None))
    )
    assertEquals(
      base.revise(
        Vector(
          StudyChange.Weighting(Weight.Duration, Weight.Uniform),
          StudyChange.Weighting(Weight.Duration, Weight.Uniform)
        )
      ),
      Left(StudyRevisionError.DuplicateField(StudyField.Weighting))
    )
  }

object StudyRevisionLawsSuite:
  type Laws   = org.typelevel.discipline.Laws#RuleSet
  type Plan   = StudyPlan[StudyKey, Px, Unit, Similarity, SignedDifference]
  type Change = StudyChange[StudyKey, Px, Unit, Similarity, SignedDifference]

  def sure[E, A](value: Either[E, A]): A = PlanCodecLawSuite.sure(value)

  val shippedTallies
      : InitialFixationLaws.Tallies[StudyKey, Px, Unit, Similarity, SignedDifference] =
    (plan, input) => plan.initialFixationTallies(input)

  /** The same change, restated to start from the plan's own value. */
  def rebase(
      plan: Plan,
      change: StudyChange[StudyKey, Px, Unit, Similarity, SignedDifference]
  ): StudyChange[StudyKey, Px, Unit, Similarity, SignedDifference] =
    val self = plan
    change match
      case StudyChange.Input(_, a)        => StudyChange.Input(self.input, a)
      case StudyChange.Layout(_, a)       => StudyChange.Layout(self.layout, a)
      case StudyChange.Method(_, _, a, p) =>
        StudyChange.Method(self.method, self.parameters, a, p)
      case StudyChange.Phases(_, _, f, r) =>
        StudyChange.Phases(self.focalPhase, self.referencePhase, f, r)
      case StudyChange.Weighting(_, a) => StudyChange.Weighting(self.weight, a)
      case StudyChange.Failures(_, a)  => StudyChange.Failures(self.policy, a)
      case StudyChange.Grid(_, a)      => StudyChange.Grid(self.grid, a)
      case StudyChange.Window(_, a)    =>
        StudyChange.Window(
          self.geometry match
            case StudyGeometry.Windowed(w, _, _) => Some(w)
            case _                               => None
          ,
          a
        )
      case StudyChange.OffWindow(_, a) =>
        StudyChange.OffWindow(
          self.geometry match
            case StudyGeometry.Windowed(_, _, p) => Some(p)
            case _                               => None
          ,
          a
        )
      case StudyChange.Scales(_, a)       => StudyChange.Scales(self.scales, a)
      case StudyChange.AngularScale(_, a) => StudyChange.AngularScale(self.angularScale, a)
      case StudyChange.Matched(_, a)      => StudyChange.Matched(self.pairing.matched, a)
      case StudyChange.Controls(_, a)     => StudyChange.Controls(self.pairing.controls, a)
      case StudyChange.Unmatched(_, a)    => StudyChange.Unmatched(self.pairing.unmatched, a)
      case StudyChange.InitialFixations(_, a) =>
        StudyChange.InitialFixations(self.initialFixations, a)

  private val otherLayout =
    StudyKey.layout(PlanCodecLawSuite.definition("eyes4s.revised-layout", 1))
  private val otherMethod =
    StudyMethod.cosine[Px](PlanCodecLawSuite.definition("eyes4s.cosine-variant", 2))

  /** A plan and a variant that changes a few of its fields, each chosen
    * independently; the variant keeps the plan whenever a combination is not
    * a valid plan, so nothing is discarded.
    */
  private def variant(a: Plan): Gen[Plan] =
    import StudyChange as C
    val otherWeight = Weight.values.find(_ != a.weight).get
    val eight       =
      StudyScale.Angular[Px](
        StudyEstimate.Gaussian[Deg](sure(Sigma.deg(8.0)), EdgePolicy.Truncate)
      )
    val options: Vector[Gen[Option[Change]]] =
      Vector(
        Gen.const(Some(C.Layout(a.layout, otherLayout))),
        Gen
          .oneOf(otherMethod, a.method)
          .map(m => Option.when(m.id != a.method.id)(C.Method(a.method, (), m, ()))),
        Gen.const(Some(C.Weighting(a.weight, otherWeight))),
        Gen.const(
          Some(C.Phases(a.focalPhase, a.referencePhase, a.referencePhase, a.focalPhase))
        ),
        Gen.const(
          Option.when(a.policy != FailurePolicy.RequireAll)(
            C.Failures(a.policy, FailurePolicy.RequireAll)
          )
        ),
        Gen.const(
          Option.when(a.angularScale.isDefined && !a.scales.contains(eight))(
            C.Scales(a.scales, a.scales :+ eight)
          )
        ),
        Gen.const(Option.when(a.scales.size > 1)(C.Scales(a.scales, a.scales.reverse))),
        Gen
          .oneOf(MatchedReferences.RequireOne, MatchedReferences.MeanOfAll)
          .map(m => Option.when(m != a.pairing.matched)(C.Matched(a.pairing.matched, m))),
        Gen.const(
          Some(
            C.Unmatched(
              a.pairing.unmatched,
              UnmatchedFocalPolicy.values.find(_ != a.pairing.unmatched).get
            )
          )
        ),
        Gen.const(
          Option.when(!a.initialFixations.isKeepAll)(
            C.InitialFixations(a.initialFixations, InitialFixationPolicy.keepAll[Px])
          )
        ),
        Gen.const(
          Option.when(a.initialFixations != InitialFixationPolicy.dropFirst[Px])(
            C.InitialFixations(a.initialFixations, InitialFixationPolicy.dropFirst[Px])
          )
        )
      )
    for
      picks   <- Gen.someOf(options.indices)
      changes <- Gen.sequence[Vector[Option[Change]], Option[Change]](
        picks.toVector.map(options)
      )
      chosen = changes.flatten.distinctBy(_.field)
    yield a.revise(chosen).getOrElse(a)

  val pairs: Gen[(Plan, Plan)] =
    val plans = PlanCodecLawSuite.initialFixationPlans
    Gen.frequency(
      1 -> Gen.zip(plans, plans),
      3 -> plans.flatMap(a => variant(a).map(a -> _)),
      1 -> plans.map(a => a -> a)
    )
