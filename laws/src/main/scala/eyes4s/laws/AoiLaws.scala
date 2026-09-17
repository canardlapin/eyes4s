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

import eyes4s.aoi.*
import eyes4s.core.*
import eyes4s.kernel.*

import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

/** A region an AOI law can move.
  *
  * [[Region]] values are opaque to a caller -- there is no public way to
  * translate one -- so the laws keep the construction of each area and rebuild
  * it on a translated frame when they need to. Only the shapes a translation
  * maps exactly are admitted: a half-open rectangle translates to a half-open
  * rectangle with the same closed edges, and an ellipse to an ellipse of the
  * same radii. A reflection would swap a rectangle's closed and open edges,
  * so the translation law is stated for translations alone.
  */
enum AoiShape[U <: Unit2D] derives CanEqual:
  case Box(lo: Pt[U], hi: Pt[U])
  case Oval(centre: Pt[U], rx: Double, ry: Double)
  case Union(a: AoiShape[U], b: AoiShape[U])
  case Intersection(a: AoiShape[U], b: AoiShape[U])

  def region: Either[GeometryError, Region[U]] = this match
    case Box(lo, hi)        => Region.rect(lo, hi)
    case Oval(c, rx, ry)    => Region.ellipse(c, rx, ry)
    case Union(a, b)        => for x <- a.region; y <- b.region yield x || y
    case Intersection(a, b) => for x <- a.region; y <- b.region yield x && y

  /** The same shape under a warp that is known to be a translation. */
  def translate(w: Warp[U, U]): Option[AoiShape[U]] = this match
    case Box(lo, hi)        => for l <- w(lo); h <- w(hi) yield Box(l, h)
    case Oval(c, rx, ry)    => w(c).map(Oval(_, rx, ry))
    case Union(a, b)        => for x <- a.translate(w); y <- b.translate(w) yield Union(x, y)
    case Intersection(a, b) =>
      for x <- a.translate(w); y <- b.translate(w) yield Intersection(x, y)

/** One area by identity and shape, before it is bound to a frame. */
final case class AoiSpec[U <: Unit2D](id: String, label: String, shape: AoiShape[U])
    derives CanEqual:
  def bind(frame: Frame[U]): Either[String, Aoi[U]] =
    for
      region <- shape.region.left.map(_.message)
      aoi    <- Aoi.of(id, label, frame, region).left.map(_.message)
    yield aoi

/** A laws-side view of one sample's membership.
  *
  * Mirrors [[SampleMembership]] but is constructible outside the aoi module,
  * so that an alternative implementation -- a mutant -- can report the same
  * shape and be run through the same laws.
  */
enum AoiMembership derives CanEqual:
  case Areas(ids: Vector[AoiId])
  case Background
  case Excluded(reason: ExclusionReason)

object AoiMembership:
  def of(m: SampleMembership): AoiMembership = m match
    case SampleMembership.Areas(ids)      => Areas(ids)
    case SampleMembership.Background      => Background
    case SampleMembership.Excluded(cause) => Excluded(cause)

/** What one assignment yields, in laws-side terms. */
final case class AoiOutcome(
    memberships: Vector[AoiMembership],
    report: AoiAssignmentReport,
    metrics: Vector[AoiMetric]
) derives CanEqual

/** The AOI accounting surface under law: assignment under a policy and the
  * per-area metrics, with the temporal support named explicitly.
  */
trait AoiAccounting:
  def run[U <: Unit2D](
      areas: AoiSet[U],
      recording: Recording[U],
      policy: MembershipPolicy,
      support: TemporalSupport
  ): Either[AoiError, AoiOutcome]

object AoiAccounting:
  val shipped: AoiAccounting = new AoiAccounting:
    def run[U <: Unit2D](
        areas: AoiSet[U],
        recording: Recording[U],
        policy: MembershipPolicy,
        support: TemporalSupport
    ): Either[AoiError, AoiOutcome] =
      areas.assign(recording, policy, support).map { assignment =>
        AoiOutcome(
          assignment.toVector.map(AoiMembership.of),
          assignment.report,
          assignment.measure.areas
        )
      }

/** A scanpath, the areas laid over its frame, and the sampled form of the
  * scanpath the AOI accounting actually consumes.
  *
  * ==The bridge from fixations to samples==
  *
  * `AoiSet.assign` accounts sample time, not fixations. To state fixation-level
  * laws the scanpath is sampled once per fixation: a `Tracked` sample at each
  * onset, and where a transition has positive duration a `Lost` sample at the
  * preceding offset, closing with a `Lost` sample at the final offset. Under
  * forward-hold support with no gap cap and a zero trailing edge, each onset
  * sample carries exactly its fixation's duration, each `Lost` sample carries
  * exactly the transition it opens, and the represented time is exactly the
  * scanpath's extent. Every law below is exact in microseconds because of this.
  *
  * The consequence for run counting is stated rather than hidden: a transition
  * of positive duration is excluded support, and excluded support ends a run,
  * so two contained fixations separated by a saccade are two runs. Only
  * abutting contained fixations share one.
  */
final class AoiScene[U <: Unit2D] private (
    val scanpath: Scanpath[U],
    val specs: Vector[AoiSpec[U]],
    val areas: AoiSet[U],
    val recording: Recording[U],
    val fixationSample: Vector[Int]
):
  def frame: Frame[U] = scanpath.frame
  def n: Int          = scanpath.n

  def contained(area: Aoi[U]): Vector[Boolean] =
    Vector.tabulate(n)(i => area.region.contains(scanpath.fixations(i).centre))

  /** The areas containing each fixation's centre, in set order. */
  def containing(i: Int): Vector[Aoi[U]] =
    areas.areas.filter(_.region.contains(scanpath.fixations(i).centre))

  def gapBefore(i: Int): Span =
    if i == 0 then Span.zero else scanpath.transitions(i - 1).duration

  def render: String =
    s"${scanpath.render} over ${areas.size} areas: " +
      specs.map(s => s"${s.id}=${s.shape}").mkString(", ")

object AoiScene:

  val support: TemporalSupport =
    TemporalSupport.ForwardHold(
      MaximumSupportGap.Unlimited,
      EdgeSupport.fixed(Span.zero).toOption.get
    )

  def of[U <: Unit2D](
      scanpath: Scanpath[U],
      specs: Vector[AoiSpec[U]]
  ): Either[String, AoiScene[U]] =
    val samples  = Vector.newBuilder[Sample[U]]
    val indices  = Vector.newBuilder[Int]
    var produced = 0
    var i        = 0
    while i < scanpath.n do
      val f = scanpath.fixations(i)
      samples += Sample(f.span.onset, Gaze.Tracked(f.centre, None))
      indices += produced
      produced += 1
      val closes =
        i == scanpath.n - 1 || scanpath.transitions(i).duration.toMicros > 0L
      if closes then
        samples += Sample(f.span.offset, Gaze.Lost())
        produced += 1
      i += 1
    for
      bound <- specs.foldLeft[Either[String, Vector[Aoi[U]]]](Right(Vector.empty)) {
        (acc, spec) => for v <- acc; a <- spec.bind(scanpath.frame) yield v :+ a
      }
      areas <- AoiSet.of(bound).left.map(_.message)
      rec   <- Recording
        .of(
          scanpath.frame,
          scanpath.clock,
          Rate.Irregular,
          Eye.Left,
          None,
          IArray.from(samples.result())
        )
        .left
        .map(_.message)
    yield new AoiScene(scanpath, specs, areas, rec, indices.result())

/** The accounting laws of the aoi module, stated over scanpaths sampled by
  * [[AoiScene]]. Each is a function of an [[AoiAccounting]] so that the same
  * property can be run against the shipped implementation and against an
  * alternative that is expected to fail it.
  */
trait AoiLaws extends Laws:

  /** The raster at which `SmallestContaining` compares areas. */
  val smallestResolution: AoiResolution = AoiResolution.of(16, 16).toOption.get

  private val smallest: MembershipPolicy =
    MembershipPolicy.SmallestContaining(smallestResolution)

  private val singleMembership: Vector[MembershipPolicy] =
    Vector(MembershipPolicy.ExclusiveByPriority, smallest)

  private val everyPolicy: Vector[MembershipPolicy] =
    MembershipPolicy.Multiple +: singleMembership

  private def outcome[U <: Unit2D](
      impl: AoiAccounting,
      scene: AoiScene[U],
      policy: MembershipPolicy
  )(check: AoiOutcome => Prop): Prop =
    impl.run(scene.areas, scene.recording, policy, AoiScene.support) match
      case Left(error)    => Prop(false) :| s"$policy: ${error.message}"
      case Right(outcome) => check(outcome) :| s"policy $policy"

  /** For each area, which fixations the policy counts it as present at. */
  private def presence[U <: Unit2D](
      scene: AoiScene[U],
      policy: MembershipPolicy
  ): Vector[(Aoi[U], Vector[Boolean])] =
    val grid = Grid.over(scene.frame, smallestResolution.nx, smallestResolution.ny).toOption
    val chosen: Vector[Vector[AoiId]] = Vector.tabulate(scene.n) { i =>
      val containing = scene.containing(i)
      policy match
        case MembershipPolicy.Multiple              => containing.map(_.id)
        case MembershipPolicy.ExclusiveByPriority   => containing.headOption.map(_.id).toVector
        case MembershipPolicy.RejectOverlap         => containing.headOption.map(_.id).toVector
        case MembershipPolicy.SmallestContaining(_) =>
          grid.toVector.flatMap { g =>
            containing.headOption.map { first =>
              containing.tail
                .foldLeft(first) { (best, c) =>
                  if c.region.area(g) < best.region.area(g) then c else best
                }
                .id
            }
          }
    }
    scene.areas.areas.map(area => area -> chosen.map(_.contains(area.id)))

  private def sum(spans: Iterable[Span]): Span = spans.foldLeft(Span.zero)(_ + _)

  private def dwellOf[U <: Unit2D](scene: AoiScene[U], present: Vector[Boolean]): Span =
    sum((0 until scene.n).filter(present).map(scene.scanpath.fixations(_).duration))

  private def runsOf[U <: Unit2D](scene: AoiScene[U], present: Vector[Boolean]): Int =
    (0 until scene.n).count { i =>
      present(i) && !(i > 0 && present(i - 1) && scene.gapBefore(i).isZero)
    }

  private def entryOf[U <: Unit2D](scene: AoiScene[U], present: Vector[Boolean]): Option[Span] =
    present.indexOf(true) match
      case -1 => None
      case i  =>
        Some(scene.scanpath.first.span.onset.until(scene.scanpath.fixations(i).span.onset))

  private def metricsAgree[U <: Unit2D](
      scene: AoiScene[U],
      policy: MembershipPolicy,
      metrics: Vector[AoiMetric]
  )(pick: (AoiMetric, Span, Option[Span], Int) => Prop): Prop =
    val byId = metrics.map(m => m.id -> m).toMap
    Prop.all(presence(scene, policy).map { (area, present) =>
      byId.get(area.id) match
        case None    => Prop(false) :| s"no metric for ${area.id.value}"
        case Some(m) =>
          pick(m, dwellOf(scene, present), entryOf(scene, present), runsOf(scene, present)) :|
            s"area ${area.id.value}"
    }*)

  def accounting[U <: Unit2D](impl: AoiAccounting, gen: Gen[AoiScene[U]]): RuleSet =
    new SimpleRuleSet(
      "aoi.accounting",
      "accounting holds under every policy" -> forAll(gen) { scene =>
        val ledger = scene.recording.representedSupport(AoiScene.support)
        Prop.all(everyPolicy.map { policy =>
          outcome(impl, scene, policy) { o =>
            Prop(o.report.accountedTime == ledger.representedTime) :|
              s"accounted ${o.report.accountedTime.toMicros} vs represented ${ledger.representedTime.toMicros}" &&
              Prop(ledger.representedTime == scene.scanpath.extent.duration) :|
              "the sampled scanpath does not represent its extent" &&
              Prop(o.report.policyCensoredTime.isZero) :| "support was censored"
          }
        }*)
      },
      "analysable time is the fixation dwell and excluded time the rest" -> forAll(gen) {
        scene =>
          val dwell = scene.scanpath.dwellTotal
          val rest  = scene.scanpath.extent.duration - dwell
          Prop.all(everyPolicy.map { policy =>
            outcome(impl, scene, policy) { o =>
              Prop(o.report.analysableTime == dwell) :|
                s"analysable ${o.report.analysableTime.toMicros} vs dwell ${dwell.toMicros}" &&
                Prop(o.report.excludedTime == rest) :|
                s"excluded ${o.report.excludedTime.toMicros} vs ${rest.toMicros}"
            }
          }*)
      },
      "union and background time do not depend on the policy" -> forAll(gen) { scene =>
        val runs =
          everyPolicy.map(p => impl.run(scene.areas, scene.recording, p, AoiScene.support))
        Prop.all(runs.zip(everyPolicy).map {
          case (Left(e), p)  => Prop(false) :| s"$p: ${e.message}"
          case (Right(o), p) =>
            runs.head match
              case Right(first) =>
                Prop(
                  o.report.aoiUnionTime == first.report.aoiUnionTime &&
                    o.report.backgroundTime == first.report.backgroundTime
                ) :| s"$p disagrees with Multiple on union or background time"
              case Left(e) => Prop(false) :| e.message
        }*)
      },
      "exclusive dwell partitions the fixation time with the background" -> forAll(gen) {
        scene =>
          Prop.all(singleMembership.map { policy =>
            outcome(impl, scene, policy) { o =>
              val total = sum(o.metrics.map(_.dwell))
              Prop(total + o.report.backgroundTime == scene.scanpath.dwellTotal) :|
                s"dwell ${total.toMicros} + background ${o.report.backgroundTime.toMicros} vs ${scene.scanpath.dwellTotal.toMicros}" &&
                Prop(total == o.report.aoiUnionTime) :| "dwell does not equal the union time" &&
                Prop(o.report.duplicatedAoiTime.isZero) :| "single membership duplicated time"
            }
          }*)
      },
      "multiple dwell sums to the union plus the duplicated time" -> forAll(gen) { scene =>
        outcome(impl, scene, MembershipPolicy.Multiple) { o =>
          val total = sum(o.metrics.map(_.dwell))
          Prop(total == o.report.aoiUnionTime + o.report.duplicatedAoiTime) :|
            s"dwell ${total.toMicros} vs union ${o.report.aoiUnionTime.toMicros} + duplicated ${o.report.duplicatedAoiTime.toMicros}" &&
            Prop(
              o.report.aoiUnionTime + o.report.backgroundTime == scene.scanpath.dwellTotal
            ) :|
            "union plus background is not the fixation dwell"
        }
      },
      "single-membership policies assign at most one area" -> forAll(gen) { scene =>
        Prop.all(singleMembership.map { policy =>
          outcome(impl, scene, policy) { o =>
            Prop.all(o.memberships.zipWithIndex.map {
              case (AoiMembership.Areas(ids), i) =>
                Prop(ids.length == 1) :| s"sample $i in ${ids.length} areas"
              case _ => Prop.passed
            }*)
          }
        }*)
      },
      "multiple assigns every containing area in set order" -> forAll(gen) { scene =>
        outcome(impl, scene, MembershipPolicy.Multiple) { o =>
          Prop.all((0 until scene.n).map { i =>
            val expected = scene.containing(i).map(_.id) match
              case ids if ids.isEmpty => AoiMembership.Background
              case ids                => AoiMembership.Areas(ids)
            Prop(o.memberships(scene.fixationSample(i)) == expected) :|
              s"fixation $i: ${o.memberships(scene.fixationSample(i))} vs $expected"
          }*)
        }
      },
      "exclusive assigns the first containing area" -> forAll(gen) { scene =>
        outcome(impl, scene, MembershipPolicy.ExclusiveByPriority) { o =>
          Prop.all((0 until scene.n).map { i =>
            val expected = scene.containing(i).headOption match
              case None    => AoiMembership.Background
              case Some(a) => AoiMembership.Areas(Vector(a.id))
            Prop(o.memberships(scene.fixationSample(i)) == expected) :|
              s"fixation $i: ${o.memberships(scene.fixationSample(i))} vs $expected"
          }*)
        }
      },
      "smallest assigns the least-area containing area, first on ties" -> forAll(gen) { scene =>
        outcome(impl, scene, smallest) { o =>
          val expected = presence(scene, smallest)
          Prop.all((0 until scene.n).map { i =>
            val ids  = expected.collect { case (a, p) if p(i) => a.id }
            val want =
              if ids.isEmpty then AoiMembership.Background else AoiMembership.Areas(ids)
            Prop(o.memberships(scene.fixationSample(i)) == want) :|
              s"fixation $i: ${o.memberships(scene.fixationSample(i))} vs $want"
          }*)
        }
      },
      "transitions are excluded support and nothing else is" -> forAll(gen) { scene =>
        Prop.all(everyPolicy.map { policy =>
          outcome(impl, scene, policy) { o =>
            val onsets = scene.fixationSample.toSet
            Prop.all(o.memberships.zipWithIndex.map { (m, k) =>
              val excluded = m == AoiMembership.Excluded(ExclusionReason.SignalLoss)
              Prop(excluded != onsets.contains(k)) :| s"sample $k: $m"
            }*)
          }
        }*)
      },
      "reject-overlap refuses exactly when a fixation lies in two areas" -> forAll(gen) {
        scene =>
          val overlapping = (0 until scene.n).find(i => scene.containing(i).length >= 2)
          val result      =
            impl.run(
              scene.areas,
              scene.recording,
              MembershipPolicy.RejectOverlap,
              AoiScene.support
            )
          (overlapping, result) match
            case (None, Right(o)) =>
              impl.run(
                scene.areas,
                scene.recording,
                MembershipPolicy.ExclusiveByPriority,
                AoiScene.support
              ) match
                case Right(exclusive) =>
                  Prop(o.memberships == exclusive.memberships) :|
                    "without an overlap, reject-overlap should agree with exclusive"
                case Left(e) => Prop(false) :| e.message
            case (None, Left(e)) => Prop(false) :| s"refused without an overlap: ${e.message}"
            case (Some(i), Right(_)) =>
              Prop(false) :| s"accepted fixation $i in ${scene.containing(i).map(_.id.value)}"
            case (Some(i), Left(AoiError.ObservedOverlap(frameId, sample, x, y, ids))) =>
              val centre = scene.scanpath.fixations(i).centre
              Prop(frameId == scene.frame.id) :| "error names another frame" &&
              Prop(sample == scene.fixationSample(i)) :|
                s"error names sample $sample, first overlap is at ${scene.fixationSample(i)}" &&
                Prop(x == centre.x && y == centre.y) :| "error names another point" &&
                Prop(ids == scene.containing(i).map(_.id)) :|
                s"error names ${ids.map(_.value)}, containing are ${scene.containing(i).map(_.id.value)}"
            case (Some(_), Left(e)) => Prop(false) :| s"wrong error: ${e.message}"
      },
      "dwell is the contained fixation time under each policy" -> forAll(gen) { scene =>
        Prop.all(everyPolicy.map { policy =>
          outcome(impl, scene, policy) { o =>
            metricsAgree(scene, policy, o.metrics) { (m, dwell, _, _) =>
              Prop(m.dwell == dwell) :| s"dwell ${m.dwell.toMicros} vs ${dwell.toMicros}" &&
              Prop(
                m.dwellProportion.contains(
                  dwell.toMicros.toDouble / scene.scanpath.dwellTotal.toMicros
                )
              ) :| s"proportion ${m.dwellProportion} for dwell ${dwell.toMicros}"
            }
          }
        }*)
      },
      "first entry is defined exactly when dwell is positive" -> forAll(gen) { scene =>
        Prop.all(everyPolicy.map { policy =>
          outcome(impl, scene, policy) { o =>
            Prop.all(o.metrics.map { m =>
              Prop(m.firstEntryLatency.isDefined == (m.dwell.toMicros > 0L)) :|
                s"${m.id.value}: entry ${m.firstEntryLatency} with dwell ${m.dwell.toMicros}"
            }*)
          }
        }*)
      },
      "first entry latency is the onset of the first contained fixation" -> forAll(gen) {
        scene =>
          Prop.all(everyPolicy.map { policy =>
            outcome(impl, scene, policy) { o =>
              metricsAgree(scene, policy, o.metrics) { (m, _, entry, _) =>
                Prop(m.firstEntryLatency == entry) :|
                  s"entry ${m.firstEntryLatency.map(_.toMicros)} vs ${entry.map(_.toMicros)}"
              }
            }
          }*)
      },
      "run count is bounded by the contained fixations" -> forAll(gen) { scene =>
        Prop.all(everyPolicy.map { policy =>
          outcome(impl, scene, policy) { o =>
            metricsAgree(scene, policy, o.metrics) { (m, dwell, _, _) =>
              val contained = presence(scene, policy)
                .collectFirst {
                  case (a, p) if a.id == m.id => p.count(identity)
                }
                .getOrElse(0)
              Prop(m.runCount >= 0 && m.runCount <= contained && contained <= scene.n) :|
                s"runs ${m.runCount}, contained $contained, n ${scene.n}" &&
                Prop((m.runCount == 0) == dwell.isZero) :|
                s"runs ${m.runCount} with dwell ${dwell.toMicros}"
            }
          }
        }*)
      },
      "run count is the number of maximal abutting blocks of contained fixations" ->
        forAll(gen) { scene =>
          Prop.all(everyPolicy.map { policy =>
            outcome(impl, scene, policy) { o =>
              metricsAgree(scene, policy, o.metrics) { (m, _, _, runs) =>
                Prop(m.runCount == runs) :| s"runs ${m.runCount} vs $runs"
              }
            }
          }*)
        },
      "the accounting is invariant under a translation of scanpath and areas" ->
        forAll(gen) { scene =>
          val b       = scene.frame.bounds
          val dx      = b.width
          val dy      = -b.height
          val shifted = Frame.of(
            FrameId(scene.frame.id.name + "-shifted"),
            Bounds.of[U](b.xMin + dx, b.yMin + dy, b.xMax + dx, b.yMax + dy).toOption.get,
            scene.frame.yAxis
          )
          val moved = for
            w <- Warp.affine(scene.frame, shifted, Mat3.translation(dx, dy)).left.map(_.message)
            sp    <- scene.scanpath.warp(w).left.map(_.message)
            specs <- scene.specs.foldLeft[Either[String, Vector[AoiSpec[U]]]](
              Right(Vector.empty)
            ) { (acc, spec) =>
              for
                v <- acc
                s <- spec.shape.translate(w).toRight(s"${spec.id} does not translate")
              yield v :+ spec.copy(shape = s)
            }
            other <- AoiScene.of(sp, specs)
          yield other
          moved match
            case Left(message) => Prop(false) :| message
            case Right(other)  =>
              Prop.all((MembershipPolicy.RejectOverlap +: everyPolicy).map { policy =>
                val here  = impl.run(scene.areas, scene.recording, policy, AoiScene.support)
                val there = impl.run(other.areas, other.recording, policy, AoiScene.support)
                (here, there) match
                  case (Right(a), Right(c)) =>
                    Prop(a == c) :| s"$policy: outcomes differ after translation"
                  case (
                        Left(AoiError.ObservedOverlap(_, i, _, _, ids)),
                        Left(AoiError.ObservedOverlap(_, j, _, _, jds))
                      ) =>
                    Prop(i == j && ids == jds) :| s"$policy: overlap differs after translation"
                  case (Left(e), Left(f)) =>
                    Prop(false) :| s"$policy: ${e.message} / ${f.message}"
                  case (Left(e), _) => Prop(false) :| s"$policy: here ${e.message}"
                  case (_, Left(f)) => Prop(false) :| s"$policy: there ${f.message}"
              }*)
        }
    )

end AoiLaws

object AoiLaws extends AoiLaws
