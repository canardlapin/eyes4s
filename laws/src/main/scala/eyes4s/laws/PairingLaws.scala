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

import scala.annotation.tailrec

/** Laws for choosing matched and control references among repeated
  * presentations of an item.
  *
  * Inputs are small trial-keyed studies in which each participant sees some
  * items one to three times in the reference phase, sometimes twice under
  * one occurrence, and recalls some of them (sometimes an item never shown).
  * So every rule meets focal trials with zero, one and several candidate
  * references, which is what decides whether a rule is implemented. A focal
  * trial with no matched reference has no controls (bead S0.7b).
  */
trait PairingLaws extends Laws:
  import PairingLaws.*

  def pairing: RuleSet =
    new SimpleRuleSet(
      "matchedReferences",
      "a study that runs has at most one matched reference per focal trial, unless it averages" ->
        forAll(genCase) { c =>
          val work = prepared(c)
          work.work() match
            case Left(_)                                                      => Prop(true)
            case Right(_) if c.pairing.matched == MatchedReferences.MeanOfAll => Prop(true)
            case Right(_)                                                     =>
              val counts = pairs(work.matched).groupBy(_._1).values.map(_.size)
              Prop(counts.forall(_ == 1)) :| s"matched pairs per focal: $counts"
        },
      "a study that runs under a one-reference rule draws one control from every other item, and none without a match" -> forAll(
        genCase
      ) { generated =>
        // The generated pairing and the default one, so the one-reference
        // rule meets items that are studied twice but never recalled.
        Prop.all(Vector(generated, generated.copy(pairing = StudyPairing.default)).map { c =>
          val work = prepared(c)
          val one  = c.pairing.matched != MatchedReferences.MeanOfAll &&
            c.pairing.controls == ControlReferences.SameSelection
          work.work() match
            case Right(_) if one =>
              val controls = pairs(work.controls)
              val focal    = unique(c.input).filter(_.phase == Focal)
              Prop.all(focal.map { f =>
                val drawn = controls.collect { case (`f`, r) => r.item }
                val items =
                  if matchable(c, f) then
                    eligible(c, f).filter(_.item != f.item).map(_.item).distinct
                  else Vector.empty
                Prop(drawn.sorted == items.sorted) :| s"$f draws $drawn, expected $items"
              }*)
            case _ => Prop(true)
        }*)
      },
      "a trial label that names two items or occurrences is a conflict, and refuses" -> forAll(
        genCase
      ) { c =>
        val keys     = c.input.trials.rows.map(_.key).distinct
        val conflict = keys
          .groupBy(k => (k.participant, k.phase, k.trial))
          .values
          .exists(g => g.map(k => (k.item, k.occurrence.value)).distinct.size > 1)
        val work = prepared(c)
        Prop(work.matchedCardinality.exists(_.itemConflicts.nonEmpty) == conflict) &&
        Prop(!conflict || work.work().isLeft)
      },
      "preflight blocks exactly when execution refuses" -> forAll(genCase) { c =>
        val p       = plan(c)
        val refused = prepared(c).work().isLeft
        Prop(p.preflight(Some(c.input)).blockers.nonEmpty == refused) :| s"refused=$refused"
      },
      "the refusal names every focal trial with several references" -> forAll(genCase) { c =>
        val work = prepared(c)
        work.matchedCardinality match
          case Left(e)            => Prop(false) :| e.message
          case Right(cardinality) =>
            val several = pairs(work.matched).groupBy(_._1).collect {
              case (k, ps) if ps.size > 1 => k
            }
            Prop(cardinality.multiple.map(_._1).toSet == several.toSet)
      },
      "MeanOfAll schedules the version-1 matched and control pairs" -> forAll(genCase) { c =>
        val averaged = prepared(c.copy(pairing = StudyPairing.version1))
        val v1       = PairingLaws.version1(c.input).prepare(c.input).toOption.get
        Prop(pairs(averaged.matched) == pairs(v1.matched)) &&
        Prop(pairs(averaged.controls) == pairs(v1.controls)) &&
        Prop(
          averaged.work().isRight ==
            averaged.matchedCardinality.exists(_.itemConflicts.isEmpty)
        ) :| "MeanOfAll refuses only a trial-identity conflict"
      },
      "SameOccurrence schedules exactly the references of the focal trial's own occurrence" -> forAll(
        genCase
      ) { c =>
        val same = c.copy(pairing =
          StudyPairing(
            MatchedReferences.SameOccurrence,
            ControlReferences.SameSelection,
            UnmatchedFocalPolicy.ReportNoMatch
          )
        )
        val work = prepared(same)
        Prop(pairs(work.matched).toSet == expected(same, matched = true)) &&
        Prop(pairs(work.controls).toSet == expected(same, matched = false))
      },
      "Select schedules exactly the chosen occurrence of each item, for matches and controls" -> forAll(
        genCase,
        genChoice
      ) { (c, choice) =>
        val chosen = c.copy(pairing =
          StudyPairing(
            MatchedReferences.Select(choice),
            ControlReferences.SameSelection,
            UnmatchedFocalPolicy.ReportNoMatch
          )
        )
        val work = prepared(chosen)
        Prop(pairs(work.matched).toSet == expected(chosen, matched = true)) &&
        Prop(pairs(work.controls).toSet == expected(chosen, matched = false))
      },
      "pairs, cardinality and refusal do not depend on input order" -> forAll(
        genCase,
        Gen.long
      ) { (c, seed) =>
        val rows     = new scala.util.Random(seed).shuffle(c.input.trials.rows)
        val shuffled = c.copy(input = StudyInput(Trials(rows)))
        val (a, b)   = (prepared(c), prepared(shuffled))
        def summary(w: PreparedStudy[TrialKey, Px, Unit, Similarity, SignedDifference]) =
          (
            pairs(w.matched).toSet,
            pairs(w.controls).toSet,
            w.matchedCardinality.map(m =>
              (
                m.multiple.map((k, rs) => k -> rs.toSet).toSet,
                m.itemConflicts.map(_.toSet).toSet
              )
            ),
            w.work().left.toOption
          )
        Prop(summary(a) == summary(b))
      },
      "controls keep every occurrence only when asked to" -> forAll(genCase) { c =>
        val all = prepared(
          c.copy(pairing = c.pairing.copy(controls = ControlReferences.AllOccurrences))
        )
        val v1 = PairingLaws.version1(c.input).prepare(c.input).toOption.get
        // Every occurrence of every other item, for the focal trials the
        // case's own rule matches.
        Prop(pairs(all.controls) == pairs(v1.controls).filter((f, _) => matchable(c, f)))
      }
    )

end PairingLaws

object PairingLaws extends PairingLaws:
  val Focal     = "retrieval"
  val Reference = "encoding"

  /** One generated study and the pairing it is prepared under. */
  final case class Case(input: StudyInput[TrialKey, Px], pairing: StudyPairing)

  /** Keys that occur once in the input; a repeated key is excluded from pairing. */
  def unique(input: StudyInput[TrialKey, Px]): Vector[TrialKey] =
    val keys = input.trials.rows.map(_.key)
    keys.filter(k => keys.count(_ == k) == 1)

  /** The references a focal trial may draw on under the case's rule,
    * computed directly from the definitions rather than the schedule.
    */
  def eligible(c: Case, focal: TrialKey): Vector[TrialKey] =
    val references =
      unique(c.input).filter(r => r.phase == Reference && r.participant == focal.participant)
    def chosen(r: TrialKey, choice: OccurrenceChoice) =
      val occurrences = references.filter(_.item == r.item).map(_.occurrence.value)
      choice match
        case OccurrenceChoice.First => r.occurrence.value == occurrences.min
        case OccurrenceChoice.Last  => r.occurrence.value == occurrences.max
        case OccurrenceChoice.At(n) => r.occurrence.value == n.value
    c.pairing.matched match
      case MatchedReferences.SameOccurrence =>
        references.filter(_.occurrence == focal.occurrence)
      case MatchedReferences.Select(choice) => references.filter(chosen(_, choice))
      case _                                => references

  /** Whether a focal trial has a matched reference under the case's rule. A
    * focal trial without one has no controls either (bead S0.7b).
    */
  def matchable(c: Case, focal: TrialKey): Boolean =
    eligible(c, focal).exists(_.item == focal.item)

  /** Every expected matched or control pair of a case whose controls follow
    * the matched rule: controls only for focal trials with a match.
    */
  def expected(c: Case, matched: Boolean): Set[(TrialKey, TrialKey)] =
    unique(c.input)
      .filter(f => f.phase == Focal && (matched || matchable(c, f)))
      .flatMap(f => eligible(c, f).filter(r => (r.item == f.item) == matched).map(f -> _))
      .toSet

  private val frame = Frame.screen("pairing", 2, 2).toOption.get
  private val grid  = Grid.over(frame, 2, 2).toOption.get

  private def path(key: TrialKey): Scanpath[Px] =
    val clock = ClockId(s"${key.participant}/${key.phase}/${key.trial}/${key.occurrence.value}")
    val fix   = Event.Fixation
      .withoutDispersion(
        Interval.of(clock, Instant.micros(0), Instant.micros(100)).toOption.get,
        Pt[Px](0.5, 1.5),
        1
      )
      .toOption
      .get
    Scanpath.of(frame, clock, IArray(fix)).toOption.get

  private def key(
      participant: String,
      phase: String,
      trial: Int,
      occurrence: Int,
      item: String
  ) =
    TrialKey
      .of(participant, phase, s"t$trial", TrialOccurrence.of(occurrence).toOption.get, item)
      .toOption
      .get

  val genChoice: Gen[OccurrenceChoice] = Gen.oneOf(
    Gen.const(OccurrenceChoice.First),
    Gen.const(OccurrenceChoice.Last),
    Gen.choose(1, 3).map(n => OccurrenceChoice.At(TrialOccurrence.of(n).toOption.get))
  )

  val genPairing: Gen[StudyPairing] = for
    matched <- Gen.oneOf(
      Gen.const(MatchedReferences.RequireOne),
      Gen.const(MatchedReferences.SameOccurrence),
      Gen.const(MatchedReferences.MeanOfAll),
      genChoice.map(MatchedReferences.Select(_))
    )
    controls  <- Gen.oneOf(ControlReferences.values.toIndexedSeq)
    unmatched <- Gen.oneOf(UnmatchedFocalPolicy.values.toIndexedSeq)
  yield StudyPairing(matched, controls, unmatched)

  /** Per participant, items shown one to three times (occasionally two
    * presentations under one occurrence) and recalled at chosen occurrences.
    */
  val genInput: Gen[StudyInput[TrialKey, Px]] =
    for
      participants <- Gen.choose(1, 2)
      trials       <- Gen.listOfN(
        participants,
        for
          items <- Gen.choose(1, 3)
          shown <- Gen.listOfN(
            items,
            Gen.oneOf(
              Gen.choose(1, 3).map(n => (1 to n).toVector),
              Gen.const(Vector(1, 1)),
              Gen.const(Vector(2))
            )
          )
          recalled <- Gen.listOfN(
            items + 1,
            Gen.oneOf(Gen.const(None), Gen.choose(1, 3).map(Some(_)))
          )
        yield (shown, recalled)
      )
      // Sometimes a row repeated verbatim, or a recall trial that reuses a
      // trial label with another item or occurrence (a conflict).
      duplicate <- Gen.frequency(4 -> Gen.const(false), 1 -> Gen.const(true))
      conflict  <- Gen.frequency(4 -> Gen.const(None), 1 -> Gen.oneOf(Some(true), Some(false)))
    yield
      var serial = 0
      def next() = { serial += 1; serial }
      val keys   = trials.zipWithIndex.flatMap { case ((shown, recalled), p) =>
        val participant = s"p$p"
        shown.zipWithIndex.flatMap { case (occurrences, i) =>
          occurrences.map(o => key(participant, Reference, next(), o, s"item$i"))
        } ++ recalled.zipWithIndex.flatMap { case (occurrence, i) =>
          occurrence.map(o => key(participant, Focal, next(), o, s"item$i"))
        }
      }
      val focal = keys.find(_.phase == Focal)
      val extra =
        (if duplicate then keys.headOption.toVector else Vector.empty) ++
          conflict.toVector.flatMap(otherItem =>
            focal.toVector.map(f =>
              TrialKey
                .of(
                  f.participant,
                  f.phase,
                  f.trial,
                  if otherItem then f.occurrence
                  else TrialOccurrence.of(f.occurrence.value + 1).toOption.get,
                  if otherItem then f.item + "-other" else f.item
                )
                .toOption
                .get
            )
          )
      StudyInput(Trials((keys.toVector ++ extra).map(k => Trial(k, (), path(k)))))

  val genCase: Gen[Case] = for
    input   <- genInput
    pairing <- genPairing
  yield Case(input, pairing)

  def plan(c: Case): StudyPlan[TrialKey, Px, Unit, Similarity, SignedDifference] =
    StudyPlan
      .configure(
        c.input.reference,
        TrialKey.layout(TrialKeyDefinitions.trialLayout),
        StudyGeometry.WholeFrame(grid),
        Focal,
        Reference,
        Weight.Duration,
        Vector(StudyScale.Native(StudyEstimate.Binned())),
        None,
        FailurePolicy.RequireAll,
        StudyMethod.cosine[Px](DefinitionId.cosine),
        (),
        c.pairing
      )
      .toOption
      .get

  def version1(
      input: StudyInput[TrialKey, Px]
  ): StudyPlan[TrialKey, Px, Unit, Similarity, SignedDifference] =
    StudyPlan
      .of(
        input.reference,
        TrialKey.layout(TrialKeyDefinitions.trialLayout),
        grid,
        Focal,
        Reference,
        Weight.Duration,
        Vector(StudyEstimate.Binned()),
        FailurePolicy.RequireAll,
        StudyMethod.cosine[Px](DefinitionId.cosine),
        ()
      )
      .toOption
      .get

  def prepared(c: Case): PreparedStudy[TrialKey, Px, Unit, Similarity, SignedDifference] =
    plan(c).prepare(c.input).toOption.get

  /** Every scheduled pair, in schedule order. */
  def pairs[K](schedule: DirectedPairSchedule[K, K]): Vector[(K, K)] =
    @tailrec
    def loop(cursor: PairCursor[K, K], acc: Vector[(K, K)]): Vector[(K, K)] =
      cursor.advance(PairQuantum.default) match
        case Right(PairPage.More(ps, _, next)) =>
          loop(next, acc ++ ps.map(p => p.left -> p.right))
        case Right(PairPage.Done(ps, _, _)) => acc ++ ps.map(p => p.left -> p.right)
        case Left(e)                        => throw new IllegalStateException(e.message)
    loop(schedule.start, Vector.empty)
