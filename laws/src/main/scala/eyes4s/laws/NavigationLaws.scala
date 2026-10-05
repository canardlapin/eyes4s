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
import eyes4s.results.*

import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Prop}
import org.typelevel.discipline.Laws

/** Laws for the provenance chain: summary > participant > query contrast >
  * pair > map > fixation > record, and back up.
  *
  * `chain` states that a cell's participants' queries are exactly the cell's
  * members, that every step down from a report's cells reaches items
  * the inspection resolves, and that every step up returns where the step
  * down started: a query's participant, a pair's query contrast, and a
  * record's fixation. It also states that a query contrast's pairs are the
  * stored pairs of its reduction, in order, with the page's total known
  * before any pair is built, and that a pair's maps are its two trials'.
  * `listings` states that offset pages concatenate to the whole listing,
  * each carrying the total, and that a trial's `usedBy` pairs name it in the
  * role they list it under, with counts that equal them, so every pair is
  * reached from its query trial and from its reference trial.
  *
  * ==Generators==
  *
  * [[cases]] draws small studies (one to three participants, two to four
  * items, one to three fixations per trial) run for real, a ledger for the
  * input, and a report of one, two or all roles, grouped by nothing, by
  * participant or by item, so cells hold one participant's queries or
  * several.
  */
object NavigationLaws extends Laws:
  type Plan = StudyPlan[StudyKey, Px, Unit, Similarity, SignedDifference]

  /** A study run over its input, its inspection, provenance and a report. */
  final case class Case(
      plan: Plan,
      input: StudyInput[StudyKey, Px],
      inspection: StudyInspection[StudyKey, Px, Similarity, SignedDifference],
      provenance: CoordinateProvenance[StudyKey, Px],
      report: Report[StudyKey]
  )

  private def get[E, A](e: Either[E, A]): A = e.fold(error => sys.error(s"$error"), identity)

  private def walk[A](first: OffsetPage[A], next: ListingOffset => OffsetPage[A]) =
    Iterator
      .iterate(Option(first))(_.flatMap(page => page.next.map(next)))
      .takeWhile(_.isDefined)
      .flatten
      .toVector

  def chain(cases: Gen[Case]): RuleSet =
    new SimpleRuleSet(
      "navigation",
      "a cell's participants' queries resolve, and each comes back to its participant" -> forAll(
        cases
      ) { c =>
        val layout = c.plan.layout
        Prop(ReportNavigation.cells(c.report).forall { cell =>
          get(ReportNavigation.participants(c.report, cell)).forall { participant =>
            participant.cell == cell &&
            get(ReportNavigation.queries(c.report, participant, layout)).forall { query =>
              c.inspection.contrastRow(query).isRight &&
              ReportNavigation.participantOf(c.report, cell, query, layout) ==
                Right(participant)
            }
          }
        })
      },
      "a cell's participants' queries are exactly the cell's members, each once" -> forAll(
        cases
      ) { c =>
        val layout = c.plan.layout
        Prop(ReportNavigation.cells(c.report).forall { ref =>
          val cell    = get(c.report.cell(ref.group, ref.role, ref.component).toRight("cell"))
          val members = cell.members.map {
            case ResultRef.Reduction(scale, _, key) => ResultRef.ContrastRow(scale, key)
            case other                              => other
          }
          val queries = get(ReportNavigation.participants(c.report, ref))
            .flatMap(p => get(ReportNavigation.queries(c.report, p, layout)))
          queries.size == members.size && queries.toSet == members.toSet
        }) :| "the participants' queries are not the cell's members"
      },
      "a query's pairs are its reductions' stored pairs, and each comes back to it" -> forAll(
        cases,
        Gen.choose(1, 4)
      ) { (c, n) =>
        val size  = get(PageSize.of(n))
        val scale = get(c.inspection.scale(0))
        Prop(scale.contrast match
          case ScaleContrast.Failed(_)  => true
          case ScaleContrast.Rows(rows) =>
            rows.references.forall { contrast =>
              val entry = get(c.inspection.contrastRow(contrast))
              StudyDesign.values.forall { design =>
                val reduction = design match
                  case StudyDesign.Matched => entry.matched
                  case StudyDesign.Control => entry.control
                reduction match
                  case None =>
                    ResultNavigation.pairs(
                      c.inspection,
                      contrast,
                      design,
                      ListingOffset.start,
                      size
                    ) ==
                      Left(NavigationError.NoReduction(contrast, design))
                  case Some(ref) =>
                    val first = get(
                      ResultNavigation.pairs(
                        c.inspection,
                        contrast,
                        design,
                        ListingOffset.start,
                        size
                      )
                    )
                    val pages = walk(
                      first,
                      o => get(ResultNavigation.pairs(c.inspection, contrast, design, o, size))
                    )
                    val pairs   = pages.flatMap(_.entries)
                    val members = get(c.inspection.reduction(ref)).members.map(_.pair)
                    pairs == members && pages.forall(_.total == members.size) &&
                    pairs.forall { pair =>
                      ResultNavigation.queryOf(pair) == Right(contrast) &&
                      (get(c.inspection.pair(pair)) match
                        case p =>
                          ResultNavigation.maps(c.inspection, pair) == Right(
                            ResultRef.Estimation(0, p.focal) -> ResultRef
                              .Estimation(0, p.reference)
                          ))
                    }
              }
            })
      },
      "a map's fixations lead to their records, and each record back to its fixation" -> forAll(
        cases
      ) { c =>
        val scale = get(c.inspection.scale(0))
        Prop(scale.estimation.references.forall { map =>
          val fixations = get(ResultNavigation.fixations(c.inspection, c.provenance, map))
          val key       = get(scale.estimation.get(map).toRight("map")).key
          val path      = c.input.trials.rows.find(_.key == key).map(_.value)
          fixations.map(_.position.value) == path.fold(Vector.empty[Int])(p =>
            (0 until p.n).toVector
          ) &&
          fixations.forall { fixation =>
            fixation.key == key &&
            ResultNavigation
              .record(c.provenance, fixation)
              .flatMap(ResultNavigation.fixationOf(c.provenance, _)) == Right(fixation)
          }
        })
      },
      "a step from the wrong level is refused, naming the reference" -> forAll(cases) { c =>
        val scale = get(c.inspection.scale(0))
        val map   = scale.estimation.references.head
        Prop(
          ResultNavigation.maps(c.inspection, map) ==
            Left(NavigationError.WrongLevel(map, NavigationLevel.Pair))
        ) &&
        Prop(
          ResultNavigation.queryOf(map) ==
            Left(NavigationError.WrongLevel(map, NavigationLevel.Pair))
        ) &&
        Prop(
          ResultNavigation.pairs(
            c.inspection,
            map,
            StudyDesign.Matched,
            ListingOffset.start,
            get(PageSize.of(1))
          ) == Left(NavigationError.WrongLevel(map, NavigationLevel.QueryContrast))
        )
      }
    )

  def listings(cases: Gen[Case]): RuleSet =
    new SimpleRuleSet(
      "listings",
      "offset pages concatenate to the whole listing, each with its total" -> forAll(
        cases,
        Gen.choose(1, 5)
      ) { (c, n) =>
        val size  = get(PageSize.of(n))
        val scale = get(c.inspection.scale(0))
        Prop(StudyDesign.values.forall { design =>
          val listing = scale.pairs(design)
          val pages   = walk(listing.page(ListingOffset.start, size), listing.page(_, size))
          pages.flatMap(_.entries) == listing.references.flatMap(listing.get) &&
          pages.forall(p => p.total == listing.total && p.entries.size <= n) &&
          listing.page(get(ListingOffset.of(listing.total + 1)), size).entries.isEmpty
        })
      },
      "a trial's usedBy pairs name it in their role, and the counts equal them" -> forAll(
        cases
      ) { c =>
        val scale                            = get(c.inspection.scale(0))
        def pairOf(ref: ResultRef[StudyKey]) = get(c.inspection.pair(ref))
        Prop(c.input.trials.rows.map(_.key).forall { key =>
          val used = scale.usedBy(key)
          scale.usedByCounts(key) == used.counts &&
          used.asQuery.forall(r => pairOf(r).focal == key) &&
          used.asMatched.forall(r =>
            pairOf(r).reference == key && pairOf(r).design == StudyDesign.Matched
          ) &&
          used.asControl
            .forall(r => pairOf(r).reference == key && pairOf(r).design == StudyDesign.Control)
        }) && Prop(StudyDesign.values.forall { design =>
          scale.pairs(design).references.forall { ref =>
            val p = pairOf(ref)
            scale.pairsOfQuery(design, p.focal).contains(ref) &&
            scale.usedBy(p.reference).asMatched.contains(ref) ==
              (design == StudyDesign.Matched)
          }
        })
      }
    )

  // ------------------------------------------------------------ generators

  private val frame = Frame.screen("navigation", 2, 2).toOption.get
  private val grid  = Grid.over(frame, 2, 2).toOption.get

  private def clock(key: StudyKey) = ClockId(s"${key.participant}/${key.stimulus}/${key.phase}")

  private def trial(key: StudyKey, points: Vector[(Double, Double)]) =
    val fixes = points.zipWithIndex.map { case ((x, y), i) =>
      get(
        Event.Fixation.withoutDispersion(
          get(
            Interval.of(clock(key), Instant.micros(i * 1000L), Instant.micros(i * 1000L + 500L))
          ),
          Pt[Px](x, y),
          1
        )
      )
    }
    Trial(key, (), get(Scanpath.of(frame, clock(key), IArray.from(fixes))))

  private val hex     = (c: Char) => Vector.fill(64)(c).mkString
  private val binding = ReportBinding(
    get(BindingDigest.parse("plan", hex('a'))),
    get(BindingDigest.parse("input", hex('b'))),
    get(BindingDigest.parse("result", hex('c'))),
    None
  )

  /** Studies, ledgers and reports as the laws' documentation describes. */
  val cases: Gen[Case] =
    val cell  = Gen.oneOf(0.5, 1.5)
    val point = Gen.zip(cell, cell)
    for
      participants <- Gen.choose(1, 3)
      items        <- Gen.choose(2, 4)
      keys =
        for
          p     <- (1 to participants).toVector
          i     <- Vector("a", "b", "c", "d").take(items)
          phase <- Vector("recall", "encode")
        yield StudyKey(s"p$p", i, phase)
      points <- Gen.listOfN(keys.size, Gen.choose(1, 3).flatMap(Gen.listOfN(_, point)))
      roles  <- Gen.oneOf(
        Vector(Role.Difference),
        Vector(Role.Matched, Role.Control),
        Vector(Role.Matched, Role.Control, Role.Difference)
      )
      grouping <- Gen.oneOf(
        Vector.empty[Grouping],
        Vector(Grouping.ByLevel(LevelTerm.Layout(LayoutField.Participant))),
        Vector(Grouping.ByLevel(LevelTerm.Layout(LayoutField.Item)))
      )
    yield
      val input = StudyInput(Trials(keys.zip(points).map((k, ps) => trial(k, ps.toVector))))
      val plan: Plan = get(
        StudyPlan.cosine(
          input.reference,
          grid,
          "recall",
          "encode",
          Weight.Duration,
          Vector(StudyEstimate.Binned()),
          FailurePolicy.RequireAll
        )
      )
      val result  = get(plan.run(input))
      val header  = Vector("participant", "image", "phase", "fixation", "x", "y")
      val records = input.trials.rows
        .flatMap(t => (0 until t.value.n).map(o => t.key -> o))
        .zipWithIndex
        .map { case ((k, o), i) => SourceRecord[StudyKey](i + 2, Disposition.Admitted(k, o)) }
      val ledger = get(
        AdmissionLedger.decide(
          SourceRef.of("navigation.csv", header, Vector.empty),
          header,
          records,
          AdmissionDecision.RequireComplete,
          AdmissionPolicy.default[StudyKey],
          Vector.empty
        )
      )
      val inspection = get(ResultInspection.study(plan, result, input, Some(ledger)))
      val provenance = get(CoordinateProvenance.of(plan, input, Some(ledger)))
      val spec       = get(
        ReportSpec.of(
          get(ReportId.of("navigation")),
          0,
          get(ReportSelection.of(roles, Vector("value"))),
          None,
          grouping,
          ReducePolicy.default,
          None,
          Spread.StandardDeviation
        )
      )
      val source = get(ReportSource.study(plan, input, result, None, binding))
      Case(plan, input, inspection, provenance, get(Report.evaluate(spec, source)))
