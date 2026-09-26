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
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*

import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Prop}
import org.typelevel.discipline.Laws

/** Laws for a study's initial-fixation policy.
  *
  * `selection` states what a selection of one trial must be, over rules and
  * scanpaths: keeping every fixation is the identity, the dropped and kept
  * fixations partition the trial by count and by duration with the kept ones
  * its suffix, and the dropped ones are exactly the ones the policy names
  * (none, the first, or the leading run whose centres lie in the closed disc
  * around the cross). `roles` states that a plan's tallies depend on the
  * trial alone, so the policy applies identically to focal and reference
  * trials.
  *
  * ==Generators hit the disc's edge==
  *
  * The disc is closed, so the positions that decide a run are the ones at
  * exactly the radius. [[InitialFixationLaws.crossCases]] uses units per
  * degree, radii and integral crosses whose products are exact, and places
  * fixations at the centre, exactly on the radius, strictly inside and
  * outside, and back inside after the run has ended, so an open disc or a
  * policy that drops every fixation inside the disc cannot pass by luck.
  */
object InitialFixationLaws extends Laws:
  /** A selection of one trial: its tally and the scanpath of the fixations
    * it keeps.
    */
  type Selection[U <: Unit2D] =
    (InitialFixationRule[U], Scanpath[U]) => (InitialFixationTally, Option[Scanpath[U]])

  /** The shipped selection. */
  def shipped[U <: Unit2D]: Selection[U] = (rule, path) =>
    val outcome = rule.select(path)
    (outcome.tally, outcome.kept)

  private def within[U <: Unit2D](cross: Pt[U], radius: Double, perDegree: Double)(
      f: Event.Fixation[U]
  ): Boolean = f.centre.distanceTo(cross) / perDegree <= radius

  def selection[U <: Unit2D](
      select: Selection[U],
      cases: Gen[(InitialFixationRule[U], Scanpath[U])]
  ): RuleSet =
    new SimpleRuleSet(
      "initialFixations",
      "keeping every fixation is the identity" -> forAll(cases) { case (_, path) =>
        InitialFixationRule.of(InitialFixationPolicy.keepAll[U], path.frame, None) match
          case Left(e)     => Prop(false) :| e.message
          case Right(keep) =>
            val (tally, kept) = select(keep, path)
            Prop(kept.exists(_ eq path)) :| "the kept scanpath is the trial's own" &&
            Prop(tally.dropped == 0 && tally.total == path.n) :| tally.render &&
            Prop(tally.droppedDuration == Span.zero)
      },
      "dropped and kept fixations partition the trial, the kept ones its suffix" -> forAll(
        cases
      ) { case (rule, path) =>
        val (tally, kept) = select(rule, path)
        val keptN         = kept.fold(0)(_.n)
        val suffix        = kept.fold(Vector.empty[Event.Fixation[U]])(_.fixations.toVector)
        Prop(tally.total == path.n) :| s"total ${tally.total} of ${path.n}" &&
        Prop(tally.dropped + tally.kept == tally.total) :| tally.render &&
        Prop(keptN == tally.kept) :| s"kept path has $keptN, tally ${tally.kept}" &&
        Prop(suffix == path.fixations.toVector.drop(tally.dropped)) :| "kept is the suffix" &&
        Prop(tally.totalDuration == path.dwellTotal) &&
        Prop(
          tally.droppedDuration + tally.keptDuration == tally.totalDuration &&
            kept.fold(Span.zero)(_.dwellTotal) == tally.keptDuration
        ) :| "durations partition the trial" &&
        Prop(kept.forall(k => k.frame == path.frame && k.clock == path.clock))
      },
      "the dropped fixations are exactly those the policy names" -> forAll(cases) {
        case (rule, path) =>
          val (tally, _) = select(rule, path)
          val fixations  = path.fixations.toVector
          rule.policy match
            case InitialFixationPolicy.KeepAll()   => Prop(tally.dropped == 0)
            case InitialFixationPolicy.DropFirst() =>
              Prop(tally.dropped == math.min(1, path.n)) :| tally.render
            case InitialFixationPolicy.DropLeadingInClosedDisc(cross, radius) =>
              rule.unitsPerDegree match
                case None => Prop(false) :| "a near-cross rule without units per degree"
                case Some(perDegree) =>
                  val inside = within(cross, radius, perDegree)
                  Prop(fixations.take(tally.dropped).forall(inside)) :|
                    "every dropped fixation lies in the closed disc" &&
                    Prop(fixations.lift(tally.dropped).forall(f => !inside(f))) :|
                    "the first kept fixation lies outside the disc"
      }
    )

  /** A plan's per-trial tallies, as a host reads them. */
  type Tallies[K, U <: Unit2D, P, S, D] =
    (StudyPlan[K, U, P, S, D], StudyInput[K, U]) => Vector[
      (K, Either[GeometryError, InitialFixationTally])
    ]

  /** Each trial's estimated map at the plan's first scale, as its values'
    * IEEE bits, or the trial's failure.
    */
  type Estimated[K, U <: Unit2D, P, S, D] =
    (StudyPlan[K, U, P, S, D], StudyInput[K, U]) => Either[
      PlanError,
      Vector[(K, Either[StudyFailure[K], Vector[Long]])]
    ]

  /** The shipped estimation: the plan run to completion. */
  def estimated[K, U <: Unit2D, P, S, D]: Estimated[K, U, P, S, D] = (plan, input) =>
    plan
      .run(input)
      .map(
        _.scales.head.estimation
          .map((k, m) => k -> m.map(_.values.toVector.map(java.lang.Double.doubleToLongBits)))
      )

  def roles[K, U <: Unit2D, P, S, D](
      tallies: Tallies[K, U, P, S, D],
      estimate: Estimated[K, U, P, S, D],
      cases: Gen[(StudyPlan[K, U, P, S, D], StudyInput[K, U])]
  )(using unit: UnitLabel[U]): RuleSet =
    new SimpleRuleSet(
      "initialFixationRoles",
      "each trial is mapped from the fixations it keeps, whatever its phase" -> forAll(cases) {
        case (plan, input) =>
          val outcomes = input.trials.rows.map(t => plan.initialFixationRule.select(t.value))
          // The same study keeping every fixation, over the kept fixations:
          // the maps every trial must have, whatever its phase.
          val trimmed = StudyInput(
            Trials(
              input.trials.rows
                .zip(outcomes)
                .map((t, o) => t.copy(value = o.kept.getOrElse(t.value)))
            )
          )(using plan.layout.digest, unit)
          val expected = plan
            .revise(
              Vector(
                StudyChange.Input(plan.input, trimmed.reference),
                StudyChange
                  .InitialFixations(plan.initialFixations, InitialFixationPolicy.keepAll[U])
              )
            )
            .left
            .map(_.message)
            .flatMap(keep => estimated(keep, trimmed).left.map(_.message))
          (estimate(plan, input).left.map(_.message), expected) match
            case (Right(found), Right(reference)) =>
              val byKey = reference.toMap
              Prop.all(found.zip(outcomes).map { case ((key, value), outcome) =>
                outcome.kept match
                  case None =>
                    Prop(
                      value == Left(
                        StudyFailure.InitialFixations(
                          key,
                          InitialFixationError.NoFixationKept(
                            outcome.tally.dropped,
                            outcome.tally.droppedDuration.toMicros
                          )
                        )
                      )
                    ) :| s"trial $key keeps no fixation: $value"
                  case Some(_) => Prop(byKey.get(key).contains(value)) :| s"trial $key"
              }*)
            case (found, reference) => Prop(false) :| s"$found / $reference"
      },
      "each trial's tally is its own selection, whatever its phase" -> forAll(cases) {
        case (plan, input) =>
          val expected = input.trials.rows.map { t =>
            t.key -> Agreement
              .frames(plan.geometry.admission, t.value.frame)
              .map(_ => plan.initialFixationRule.select(t.value).tally)
          }
          Prop(tallies(plan, input) == expected)
      },
      "exchanging the focal and reference phases leaves every tally unchanged" -> forAll(
        cases
      ) { case (plan, input) =>
        val swapped = plan.revise(
          Vector(
            StudyChange.Phases(
              plan.focalPhase,
              plan.referencePhase,
              plan.referencePhase,
              plan.focalPhase
            )
          )
        )
        swapped match
          case Left(e)      => Prop(false) :| e.message
          case Right(other) => Prop(tallies(plan, input) == tallies(other, input))
      }
    )

  // -------------------------------------------------------------------------
  // Generators
  // -------------------------------------------------------------------------

  private val clock = ClockId("initial-fixations")

  /** Fixations at `centres`, laid end to end with 10-microsecond gaps. The
    * spans are ordered and positive by construction, so every value is a
    * scanpath; nothing is discarded.
    */
  private def fixations(centres: Vector[Pt[Px]], durations: Vector[Long]): Scanpath[Px] =
    val built = centres.indices.map { i =>
      val onset = durations.take(i).sum + i * 10L
      val span  =
        Interval
          .of(clock, Instant.micros(onset), Instant.micros(onset + durations(i)))
          .toOption
          .get
      Event.Fixation.withoutDispersion(span, centres(i), 1).toOption.get
    }
    Scanpath.of(screen, clock, IArray.from(built)).toOption.get

  /** The screen every generated trial lies on: 1920 x 1080 pixels. */
  val screen: Frame[Px] =
    Frame.of(
      FrameId("initial-fixation-screen"),
      Bounds.of[Px](0, 0, 1920, 1080).toOption.get,
      YAxis.Down
    )

  private val perDegrees = Gen.oneOf(2.0, 35.0, 40.0, 64.0)
  private val radii      = Gen.oneOf(0.5, 1.5, 2.0, 0.25)

  /** A centre at `fraction` of the radius from the cross, in a direction
    * along an axis so that the radius itself is exact.
    */
  private def around(cross: Pt[Px], distance: Double, axis: Int): Pt[Px] = axis match
    case 0 => Pt[Px](cross.x + distance, cross.y)
    case 1 => Pt[Px](cross.x - distance, cross.y)
    case 2 => Pt[Px](cross.x, cross.y + distance)
    case _ => Pt[Px](cross.x, cross.y - distance)

  /** A trial against a cross: a leading run inside the closed disc (at the
    * centre, exactly on the radius or strictly inside), then fixations
    * outside, sometimes returning inside later.
    */
  def trialNear(cross: Pt[Px], radius: Double, perDegree: Double): Gen[Scanpath[Px]] =
    val edge                = radius * perDegree
    val inside: Gen[Pt[Px]] = for
      axis <- Gen.choose(0, 3)
      d    <- Gen.oneOf(Gen.const(0.0), Gen.const(edge), Gen.choose(0.0, edge))
    yield around(cross, d, axis)
    val outside: Gen[Pt[Px]] = for
      axis <- Gen.choose(0, 3)
      d    <- Gen.oneOf(Gen.const(edge + 1.0), Gen.choose(edge * 1.01 + 0.01, edge * 4 + 50))
    yield around(cross, d, axis)
    for
      lead      <- Gen.choose(0, 4)
      rest      <- Gen.choose(0, 5)
      leading   <- Gen.listOfN(lead, inside)
      first     <- outside
      following <- Gen.listOfN(rest, Gen.oneOf(inside, outside))
      centres =
        (leading ++ (if lead + rest == 0 then List(first) else first :: following)).toVector
      durations <- Gen.listOfN(centres.size, Gen.choose(1L, 400000L))
    yield fixations(centres, durations.toVector)

  /** Rules under every policy, each with a trial against its cross. */
  val crossCases: Gen[(InitialFixationRule[Px], Scanpath[Px])] =
    for
      perDegree <- perDegrees
      radius    <- radii
      x         <- Gen.choose(200, 1700)
      y         <- Gen.choose(200, 900)
      cross = Pt[Px](x.toDouble, y.toDouble)
      scale = LinearAngularScale.of(screen, perDegree).toOption
      near  = InitialFixationPolicy.dropLeadingInClosedDisc(cross, radius).toOption.get
      policy <- Gen.frequency(
        1 -> Gen.const(InitialFixationPolicy.keepAll[Px]),
        1 -> Gen.const(InitialFixationPolicy.dropFirst[Px]),
        4 -> Gen.const(near)
      )
      rule = InitialFixationRule.of(policy, screen, scale).toOption.get
      path <- trialNear(cross, radius, perDegree)
    yield (rule, path)

  private type Cosine = StudyPlan[StudyKey, Px, Unit, Similarity, SignedDifference]

  /** Plans under every policy with an input of two participants, two items
    * and both phases, every trial against the plan's cross.
    */
  val planCases: Gen[(Cosine, StudyInput[StudyKey, Px])] =
    for
      (rule, _) <- crossCases
      (cross, radius, perDegree) = rule.policy match
        case InitialFixationPolicy.DropLeadingInClosedDisc(c, r) =>
          (c, r, rule.unitsPerDegree.getOrElse(35.0))
        case _ => (Pt[Px](960, 540), 1.5, 35.0)
      keys =
        for
          participant <- Vector("p1", "p2")
          item        <- Vector("a", "b")
          phase       <- Vector("recall", "encode")
        yield StudyKey(participant, item, phase)
      paths <- Gen.listOfN(keys.size, trialNear(cross, radius, perDegree))
      input = StudyInput(Trials(keys.zip(paths).map((k, p) => Trial(k, (), p))))
      plan  = StudyPlan
        .configure(
          input.reference,
          StudyKey.layout(DefinitionId.studyLayout),
          StudyGeometry.WholeFrame(Grid.over(screen, 16, 9).toOption.get),
          "recall",
          "encode",
          Weight.Duration,
          Vector(StudyScale.Native(StudyEstimate.Binned())),
          LinearAngularScale.of(screen, perDegree).toOption,
          FailurePolicy.RequireAll,
          StudyMethod.cosine[Px](DefinitionId.cosine),
          (),
          StudyPairing.default,
          rule.policy
        )
        .toOption
        .get
    yield (plan, input)
