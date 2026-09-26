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

/** Laws for the coordinate provenance of a study input's fixations and the
  * pages of its source records.
  *
  * `trails` states what every admitted fixation's trail must be, over plans,
  * inputs and ledgers: its admitted position is the input's; its window
  * position is the window's entry of that position; its placement partitions
  * each trial exactly as the plan's initial-fixation and window tallies count
  * it; and its degrees are measured from the centre of the window (or the
  * screen), `x` right and `y` up, at the plan's units per degree, within
  * `tol`. `pages` states that paging a ledger's records from the first page
  * yields every record once, in record order, that every page carries the
  * ledger's count, that a page asked from a listed record starts there, and
  * that an admitted record's view is the provenance of the fixation it
  * supplied.
  *
  * ==Generators==
  *
  * [[cases]] draws windows with either axis direction (from
  * [[WindowLaws.genWindow]]) and fixations on and beside the window's and the
  * screen's half-open edges, so a closed window, a placement decided in the
  * wrong order, or degrees with `y` down cannot pass by luck. Plans keep every
  * fixation or drop the first; some have no angular scale. Ledgers interleave
  * rejected records with the admitted ones and sometimes carry a correction
  * rule.
  */
object CoordinateProvenanceLaws extends Laws:
  type Plan = StudyPlan[StudyKey, Px, Unit, Similarity, SignedDifference]

  /** A plan, the input it was made for, and a ledger of that input. */
  final case class Case(
      plan: Plan,
      input: StudyInput[StudyKey, Px],
      ledger: AdmissionLedger[StudyKey]
  ):
    def provenance: CoordinateProvenance[StudyKey, Px] =
      CoordinateProvenance
        .of(plan, input, Some(ledger))
        .fold(e => sys.error(e.message), identity)

  private def get[E, A](e: Either[E, A]): A = e.fold(error => sys.error(s"$error"), identity)

  private def fixations(c: Case): Vector[(StudyKey, Scanpath[Px], Int)] =
    c.input.trials.rows.flatMap(t => (0 until t.value.n).map(i => (t.key, t.value, i)))

  private def position(i: Int): ScanpathPosition = get(ScanpathPosition.of(i))

  def trails(tol: Tolerance, cases: Gen[Case]): RuleSet =
    new SimpleRuleSet(
      "coordinateProvenance",
      "a fixation's admitted position is the input's, in its frame" -> forAll(cases) { c =>
        val p = c.provenance
        Prop(fixations(c).forall { (key, path, i) =>
          get(p.fixation(key, position(i))).trail.admitted ==
            FramedPosition(path.frame.id, path.fixations(i).centre)
        })
      },
      "placements partition each trial as the plan's tallies count it" -> forAll(cases) { c =>
        val p        = c.provenance
        val windows  = c.plan.windowTallies(c.input).toMap
        val initials = c.plan.initialFixationTallies(c.input).toMap
        Prop(c.input.trials.rows.forall { trial =>
          val placements = (0 until trial.value.n).map(i =>
            get(p.fixation(trial.key, position(i))).trail.placement
          )
          val window  = get(windows(trial.key))
          val initial = get(initials(trial.key))
          // The study fails a trial when nothing would be mapped, or when the
          // plan fails trials with any fixation outside the window.
          val failTrial = c.plan.geometry match
            case StudyGeometry.Windowed(_, _, OffWindowPolicy.FailTrial) => true
            case _                                                       => false
          val fails = window.allOutside || (failTrial && window.outsideWindow > 0)
          placements.count(_ == MapPlacement.DroppedInitial) == initial.dropped &&
          placements.count(_ == MapPlacement.OutsideScreen) == window.outsideScreen &&
          placements.count {
            case MapPlacement.OutsideWindow(_) => true
            case _                             => false
          } == window.outsideWindow &&
          placements.count(_ == MapPlacement.InMap) == (if fails then 0 else window.inside) &&
          placements.count(_ == MapPlacement.TrialFailed(window)) ==
            (if fails then window.inside else 0) &&
            placements.take(initial.dropped).forall(_ == MapPlacement.DroppedInitial)
        }) :| "a placement count differs from the plan's tally"
      },
      "the window position is the window's entry of the admitted position" -> forAll(cases) {
        c =>
          val p = c.provenance
          Prop(fixations(c).forall { (key, path, i) =>
            val trail  = get(p.fixation(key, position(i))).trail
            val centre = path.fixations(i).centre
            c.plan.geometry match
              case StudyGeometry.Windowed(w, _, _) =>
                val expected = w.locate(centre) match
                  case HalfOpenPlacement.Inside(local) => local
                  case HalfOpenPlacement.Outside(_)    => get(w.enter(centre).toRight("enter"))
                trail.window == Some(FramedPosition(w.frame.id, expected)) &&
                (trail.placement match
                  case MapPlacement.InMap | MapPlacement.TrialFailed(_) => true
                  case _                                                => false
                ) ==
                  (i >= get(c.plan.initialFixationTallies(c.input).toMap.apply(key)).dropped &&
                    w.locate(centre).isInside)
              case StudyGeometry.WholeFrame(_) => trail.window.isEmpty
          })
      },
      "degrees run from the measured frame's centre, x right and y up" -> forAll(cases) { c =>
        val p = c.provenance
        (c.plan.angularScale, p.angular) match
          case (None, None) =>
            Prop(
              fixations(c).forall((key, _, i) =>
                get(p.fixation(key, position(i))).trail.angular.isEmpty
              )
            )
          case (Some(scale), Some(reference)) =>
            val measured = c.plan.geometry match
              case StudyGeometry.Windowed(w, _, _) => w.frame
              case StudyGeometry.WholeFrame(g)     => g.frame
            val ppd = scale.unitsPerDegree
            Prop(reference.measured == measured.id) &&
            Prop(reference.origin == measured.centre) &&
            Prop(reference.unitsPerDegree == ppd) &&
            Prop(reference.degrees.yAxis == YAxis.Up) &&
            Prop(reference.degrees.id == FrameId(s"${measured.id.name}/degrees")) &&
            Prop(fixations(c).forall { (key, path, i) =>
              val trail = get(p.fixation(key, position(i))).trail
              val local = trail.window.fold(path.fixations(i).centre)(_.position)
              val up    = if measured.yAxis == YAxis.Up then 1.0 else -1.0
              val dx    = (local.x - measured.centre.x) / ppd
              val dy    = up * (local.y - measured.centre.y) / ppd
              trail.angular.exists(a =>
                a.reference == reference &&
                  tol.approxEquals(a.position, Pt[Unit2D.Deg](dx, dy))
              )
            }) :| "a fixation's degrees are not its offset from the centre"
          case (scale, reference) => Prop(false) :| s"scale $scale, reference $reference"
      }
    )

  def pages(cases: Gen[Case]): RuleSet =
    new SimpleRuleSet(
      "sourceRecordPages",
      "pages from the first give every record once, in order, each with the total" -> forAll(
        cases,
        Gen.choose(1, 7)
      ) { (c, n) =>
        val listing = c.provenance.records
        val size    = get(PageSize.of(n))
        def walk(
            page: RecordPage[StudyKey, Px],
            seen: Vector[RecordPage[StudyKey, Px]]
        ): Vector[RecordPage[StudyKey, Px]] =
          page.next.fold(seen :+ page)(next =>
            walk(get(listing.page(next, size)), seen :+ page)
          )
        val all     = walk(get(listing.first(size)), Vector.empty)
        val records = all.flatMap(_.entries.map(_.record.csv.value))
        Prop(listing.total == c.ledger.records.size) &&
        Prop(all.forall(_.total == listing.total)) &&
        Prop(all.forall(_.entries.size <= n)) &&
        Prop(records == c.ledger.records.map(_.record)) :| s"paged $records" &&
        Prop(all.flatMap(_.entries.map(_.entry)) == c.ledger.records)
      },
      "a page asked from inside a gap starts at the next listed record" -> forAll(cases) { c =>
        val listing  = c.provenance.records
        val size     = get(PageSize.of(2))
        val listed   = c.ledger.records.map(_.record)
        val unlisted = (2 to listed.last + 1).filterNot(listed.contains)
        Prop(unlisted.forall { number =>
          val from = get(get(CsvRecord.of(number)).dataRecord)
          listed.find(_ > number) match
            case Some(next) =>
              get(listing.page(from, size)).entries.headOption.map(_.record.csv.value) ==
                Some(next)
            case None =>
              listing.page(from, size) == Left(ProvenanceError.PageStart(from, listed.size))
        }) :| s"unlisted $unlisted of $listed"
      },
      "a page asked from a listed record starts there" -> forAll(cases) { c =>
        val listing = c.provenance.records
        val size    = get(PageSize.of(3))
        Prop(c.ledger.records.forall { entry =>
          val record = get(get(CsvRecord.of(entry.record)).dataRecord)
          get(listing.page(record, size)).entries.headOption.map(_.record) == Some(record)
        })
      },
      "an admitted record's view is its fixation's provenance" -> forAll(cases) { c =>
        val p    = c.provenance
        val page = get(p.records.first(get(PageSize.of(PageSize.maximum))))
        Prop(page.entries.forall { view =>
          view.entry.disposition match
            case Disposition.Admitted(key, _) =>
              val source =
                get(p.sources.trial(key)).admitted.find(_.record == view.entry.record)
              source.exists(s =>
                view.fixation == Some(get(p.fixation(key, position(s.index)))) &&
                  view.fixation.exists(_.record == Right(view.record))
              )
            case Disposition.Rejected(_, _, _) => view.fixation.isEmpty
        })
      }
    )

  // ------------------------------------------------------------ generators

  private def coordinate(
      lo: Double,
      hi: Double,
      outerLo: Double,
      outerHi: Double
  ): Gen[Double] =
    WindowLaws.straddling(lo, hi, outerLo, outerHi)

  private def path(key: StudyKey, screen: Frame[Px], points: Vector[Pt[Px]]): Scanpath[Px] =
    val clock = ClockId(s"${key.participant}/${key.stimulus}/${key.phase}")
    val fixes = points.zipWithIndex.map { (p, i) =>
      get(
        Event.Fixation.withoutDispersion(
          get(Interval.of(clock, Instant.micros(i * 1000L), Instant.micros(i * 1000L + 400L))),
          p,
          1
        )
      )
    }
    get(Scanpath.of(screen, clock, IArray.from(fixes)))

  /** Plans, inputs and ledgers as the laws' documentation describes. */
  val cases: Gen[Case] =
    for
      window    <- WindowLaws.genWindow
      windowed  <- Gen.oneOf(true, false)
      failTrial <- Gen.oneOf(true, false)
      screen     = window.parent
      b          = screen.bounds
      w          = window.region
      straddling =
        for
          x <- Gen.oneOf(
            coordinate(w.xMin, w.xMax, b.xMin - 50, b.xMax + 50),
            coordinate(b.xMin, b.xMax, b.xMin - 50, b.xMax + 50)
          )
          y <- Gen.oneOf(
            coordinate(w.yMin, w.yMax, b.yMin - 50, b.yMax + 50),
            coordinate(b.yMin, b.yMax, b.yMin - 50, b.yMax + 50)
          )
        yield Pt[Px](x, y)
      inside = Gen
        .zip(
          Gen.choose(w.xMin, java.lang.Math.nextDown(w.xMax)),
          Gen.choose(w.yMin, java.lang.Math.nextDown(w.yMax))
        )
        .map((x, y) => Pt[Px](x, y))
      // On the screen but left or right of the window, when there is room.
      beside = Vector(
        Option.when(w.xMin > b.xMin)(Gen.choose(b.xMin, java.lang.Math.nextDown(w.xMin))),
        Option.when(w.xMax < b.xMax)(Gen.choose(w.xMax, java.lang.Math.nextDown(b.xMax)))
      ).flatten
      outsideWindow =
        if beside.isEmpty then straddling
        else
          Gen
            .zip(
              Gen.oneOf(beside).flatMap(identity),
              Gen.choose(b.yMin, java.lang.Math.nextDown(b.yMax))
            )
            .map((x, y) => Pt[Px](x, y))
      // Trials mix fixations in, beside and around the window, so a trial
      // with exactly one fixation outside it is common.
      point = Gen.frequency(3 -> inside, 2 -> outsideWindow, 3 -> straddling)
      keys  = Vector(
        StudyKey("p1", "a", "recall"),
        StudyKey("p1", "a", "encode"),
        StudyKey("p2", "b", "recall")
      )
      points   <- Gen.listOfN(keys.size, Gen.choose(1, 5).flatMap(Gen.listOfN(_, point)))
      ppd      <- Gen.option(WindowLaws.genScaleValue)
      drop     <- Gen.oneOf(true, false)
      rejected <- Gen.listOfN(keys.size + 1, Gen.choose(0, 2))
      rule     <- Gen.option(Gen.oneOf(Correction.FlipX, Correction.FlipY))
      gaps     <- Gen.listOfN(40, Gen.frequency(4 -> Gen.const(0), 1 -> Gen.choose(1, 3)))
    yield
      val trials   = keys.zip(points).map((k, ps) => Trial(k, (), path(k, screen, ps.toVector)))
      val input    = StudyInput(Trials(trials))
      val geometry =
        if windowed then
          get(
            StudyGeometry
              .windowed(
                window,
                get(Grid.over(window.frame, 4, 3)),
                if failTrial then OffWindowPolicy.FailTrial else OffWindowPolicy.Exclude
              )
          )
        else StudyGeometry.WholeFrame(get(Grid.over(screen, 4, 3)))
      val plan: Plan = get(
        StudyPlan.configure(
          input.reference,
          StudyKey.layout(DefinitionId.studyLayout),
          geometry,
          "recall",
          "encode",
          Weight.Duration,
          Vector(StudyScale.Native(StudyEstimate.Binned())),
          ppd.map(v => get(LinearAngularScale.of(screen, v))),
          FailurePolicy.RequireAll,
          StudyMethod.cosine[Px](DefinitionId.cosine),
          (),
          initialFixations =
            if drop then InitialFixationPolicy.dropFirst[Px]
            else InitialFixationPolicy.keepAll[Px]
        )
      )
      // Records in trial order, a run of rejected records before each trial;
      // record numbers sometimes skip, as a hand-built ledger may.
      val raw     = Vector("x")
      val grouped = trials.zipWithIndex.flatMap { (t, i) =>
        Vector.fill(rejected(i))(Left(()): Either[Unit, (StudyKey, Int)]) ++
          (0 until t.value.n).map(o => Right(t.key -> o))
      } ++ Vector.fill(rejected.last)(Left(()): Either[Unit, (StudyKey, Int)])
      val numbers = grouped.indices.map(i => i + 2 + gaps.take(i + 1).sum)
      val records = grouped.zip(numbers).map { (entry, number) =>
        SourceRecord[StudyKey](
          number,
          entry.fold(
            _ => Disposition.Rejected(raw, None, AdmissionReason.Width(4, 1)),
            (k, o) => Disposition.Admitted(k, o)
          )
        )
      }
      val policy = AdmissionPolicy[StudyKey](
        OffScreenPolicy.ExcludeRecord,
        rule.toVector.map(c => AppliedCorrection(CorrectionScope.AllTrials[StudyKey](), c))
      )
      val header = Vector("participant", "image", "phase", "x", "y")
      val ledger = get(
        AdmissionLedger.decide(
          SourceRef.of("generated.csv", header, Vector.empty),
          header,
          records,
          AdmissionDecision.ReviewExclusions,
          policy,
          Vector.empty
        )
      )
      Case(plan, input, ledger)
