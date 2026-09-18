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

import eyes4s.codec.*
import eyes4s.compare.Similarity
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import eyes4s.surface.EdgePolicy
import org.scalacheck.{Gen, Test}

/** Published verified-resolution laws over generated saved graphs (plan,
  * input, ledger, result, temporal input with its base by reference,
  * recording input and packed recording with payloads), with deliberate
  * writer mutants that the laws must kill by a falsified property.
  */
class ManifestLawSuite extends munit.DisciplineSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)

  /** Generated operands satisfy every constructor invariant by construction,
    * so a `Left` here is a generator bug rather than a discarded case.
    */
  private def sure[E, A](value: Either[E, A]): A =
    value.fold(
      e => throw new IllegalStateException(s"generator invariant violated: $e"),
      identity
    )

  private val frame     = get(Frame.screen("law-display", 2, 2))
  private val baseFrame = get(Frame.screen("law-base", 2, 2))
  private val grid      = get(Grid.over(frame, 2, 2))
  private val display   = get(Frame.screen("law-recording", 1000, 1000))
  private val studies   = StudyCodecs.cosine[Px]
  private val inputs    = StudyInputCodecs.study[Px]
  private val results   = StudyResultCodecs.cosine[Px]
  private val temporals = TemporalInputCodecs.study[Px](StudyEmbedding.ByReference)
  private val decoders  = get(ArtifactDecoders.study[Px])

  private final case class Graph(
      plan: StudyPlan[StudyKey, Px, Unit, Similarity, SignedDifference],
      input: StudyInput[StudyKey, Px],
      ledger: AdmissionLedger[StudyKey],
      result: StudyResult[StudyKey, Px, Similarity, SignedDifference],
      base: StudyInput[StudyKey, Px],
      temporal: TemporalStudyInput[StudyKey, Px],
      recording: Recording[Px],
      recordingInput: RecordingInput[Px]
  )

  private def clock(key: StudyKey): ClockId =
    ClockId(s"${key.participant}/${key.stimulus}/${key.phase}")

  private def trial(key: StudyKey, in: Frame[Px], points: Vector[(Double, Double)]) =
    val fixes = points.zipWithIndex.map { case ((x, y), i) =>
      sure(
        Event.Fixation.withoutDispersion(
          sure(
            Interval
              .of(clock(key), Instant.micros(i * 2000L), Instant.micros(i * 2000L + 1000L))
          ),
          Pt[Px](x, y),
          1 + i
        )
      )
    }
    Trial(key, (), sure(Scanpath.of(in, clock(key), IArray.from(fixes))))

  private val allKeys: Vector[StudyKey] =
    for
      p <- Vector("p1", "p2")
      s <- Vector("a", "b")
      f <- Vector("recall", "encode")
    yield StudyKey(p, s, f)

  /** Unique keys, the first always present, one to three fixations each. */
  private def studyInputs(in: Frame[Px]): Gen[StudyInput[StudyKey, Px]] = for
    mask <- Gen.listOfN(allKeys.size - 1, Gen.oneOf(true, false))
    keys = allKeys.head +: allKeys.tail.zip(mask).collect { case (k, true) => k }
    points <- Gen.listOfN(
      keys.size,
      Gen
        .choose(1, 3)
        .flatMap(n => Gen.listOfN(n, Gen.zip(Gen.oneOf(0.5, 1.5), Gen.oneOf(0.5, 1.5))))
    )
  yield StudyInput(Trials(keys.zip(points).map((k, p) => trial(k, in, p.toVector))))

  /** Every fixation admitted, in trial order: the ledger of this input. */
  private def ledgerOf(input: StudyInput[StudyKey, Px]): AdmissionLedger[StudyKey] =
    val admitted = input.trials.rows.flatMap(t => (0 until t.value.n).map(o => t.key -> o))
    val raw    = admitted.map((k, o) => Vector(k.participant, k.stimulus, k.phase, o.toString))
    val header = Vector("participant", "image", "phase", "fixation")
    sure(
      AdmissionLedger.of(
        SourceRef.of("law.csv", header, raw),
        header,
        admitted.zipWithIndex.map { case ((k, o), i) =>
          SourceRecord(i + 2, Disposition.Admitted(k, o))
        },
        AdmissionOutcome.Complete
      )
    )

  private def temporalOf(base: StudyInput[StudyKey, Px], anchored: Vector[Boolean]) =
    val epochs = base.trials.rows.zip(anchored).collect { case (t, true) =>
      val c = clock(t.key)
      t.key -> TrialEpoch(
        Instant.micros(0L),
        sure(
          ObservedCoverage.of(
            c,
            Vector(sure(Interval.of(c, Instant.micros(0L), Instant.micros(t.value.n * 2000L))))
          )
        )
      )
    }
    sure(TemporalStudyInput.of(base, epochs))

  private val lineages: Gen[SampleLineage] = for
    basis <- Gen.oneOf(SampleLineage.measured, SampleLineage.interpolated)
    steps <- Gen.choose(0, 2).flatMap(n => Gen.listOfN(n, Gen.oneOf(true, false)))
  yield steps.foldLeft(basis)((acc, smoothed) =>
    if smoothed then acc.smoothed else acc.projected
  )

  private val gazes: Gen[Gaze[Px]] = Gen.frequency(
    5 -> (for
      x <- Gen.choose(0.0, 999.0)
      y <- Gen.choose(0.0, 999.0)
      p <- Gen.option(Gen.choose(0.5, 5000.0))
    yield Gaze.Tracked(Pt[Px](x, y), p)),
    1 -> Gen.const(Gaze.Blink[Px]()),
    1 -> Gen.const(Gaze.Lost[Px]()),
    1 -> Gen.choose(1000.5, 2000.0).map(x => Gaze.OffScreen[Px](Pt[Px](x, 10.0)))
  )

  /** Irregular timestamps from origins beyond JavaScript's exact integer range. */
  private val recordings: Gen[Recording[Px]] = for
    n       <- Gen.choose(1, 12)
    origin  <- Gen.oneOf(0L, 9007199254740993L, Long.MinValue + 1)
    gaps    <- Gen.listOfN(n, Gen.choose(1L, 5000L))
    gaze    <- Gen.listOfN(n, gazes)
    lineage <- Gen.listOfN(n, lineages)
    times = gaps.scanLeft(origin)(_ + _).tail
  yield sure(
    Recording.of(
      display,
      ClockId("law-tracker"),
      Rate.Irregular,
      Eye.Left,
      Some(PupilUnit.Arbitrary),
      IArray
        .from(times.indices.map(i => Sample(Instant.micros(times(i)), gaze(i), lineage(i)))),
      sure(SamplingTolerance.of(Span.micros(2)))
    )
  )

  private val scales: Gen[Vector[StudyEstimate[Px]]] = Gen.oneOf(
    Vector(StudyEstimate.Binned[Px]()),
    Vector(
      StudyEstimate.Binned[Px](),
      StudyEstimate.Gaussian(sure(Sigma.px(0.5)), EdgePolicy.Truncate)
    )
  )

  private val graphs: Gen[Graph] = for
    input     <- studyInputs(frame)
    base      <- studyInputs(baseFrame)
    anchored  <- Gen.listOfN(base.trials.rows.size, Gen.oneOf(true, false))
    estimates <- scales
    recording <- recordings
    plan = sure(
      StudyPlan.cosine[Px](
        input.reference,
        grid,
        "recall",
        "encode",
        Weight.Duration,
        estimates,
        FailurePolicy.RequireAll
      )
    )
  yield Graph(
    plan,
    input,
    ledgerOf(input),
    sure(plan.run(input)),
    base,
    temporalOf(base, anchored.toVector),
    recording,
    sure(
      RecordingInput
        .of(RecordingRef("law-recording"), RecordingChannels.Monocular(recording), None, None)
    )
  )

  private def name(value: String): ArtifactName = get(ArtifactName.of(value))

  /** The lawful writer: every artifact through its registered codec. */
  private def write(g: Graph): Either[CodecError, StoredGraph] = for
    plan     <- StoredArtifact.plan("plan", studies, g.plan)
    input    <- StoredArtifact.input("input", inputs, g.input)
    ledger   <- StoredArtifact.ledger("ledger", inputs, g.ledger)
    result   <- StoredArtifact.result("result", results, g.result)
    base     <- StoredArtifact.input("base", inputs, g.base)
    temporal <- StoredArtifact.temporalInput("temporal", temporals, g.temporal)
    recorded <- StoredArtifact.recordingInput("recording-input", g.recordingInput)
    packed   <- StoredArtifact.packedRecording("recording", g.recording)
    saved    <- SavedManifest.of(
      Vector(
        plan,
        input,
        ledger,
        result,
        base,
        temporal,
        recorded,
        packed.recording
      ) ++ packed.payloads,
      Vector(
        ManifestRelation.PlanInput(plan.name, input.name),
        ManifestRelation.ResultOf(result.name, plan.name, input.name),
        ManifestRelation.LedgerOf(ledger.name, input.name),
        ManifestRelation.TemporalBase(temporal.name, base.name),
        ManifestRelation.RecordingOf(recorded.name, packed.recording.name)
      ) ++ packed.relations
    )
  yield StoredGraph.of(saved)

  private def reproduces(g: Graph, r: ResolvedManifest[StudyKey, Px]): Boolean =
    r.plans.map(_._2.description) == Vector(g.plan.description) &&
      r.inputs.map(_._2.reference) == Vector(g.input.reference, g.base.reference) &&
      r.ledgers.map(_._2) == Vector(g.ledger) &&
      r.results.map(_._2.encode) == Vector(results.codec.encode(g.result)) &&
      r.temporalInputs.map(_._2.reference) == Vector(g.temporal.reference) &&
      r.recordingInputs.map(_._2.reference) == Vector(g.recordingInput.reference) &&
      r.recordings.map(_._2.contentHash) == Vector(g.recording.contentHash) &&
      r.payloads.size == 4

  private val laws = ManifestLaws.verifiedResolution(graphs, write, decoders, reproduces)

  checkAll("saved study graph", laws)

  private val parameters = Test.Parameters.default.withMinSuccessfulTests(40)

  private def outcome(
      ruleSet: ManifestLaws.RuleSet,
      property: String
  ): Test.Status =
    ruleSet.all.properties
      .collectFirst { case (label, prop) if label.endsWith(property) => prop }
      .map(prop => Test.check(parameters, prop).status)
      .getOrElse(fail(s"no property $property"))

  /** Killed only by a falsified resolution property, never by exhaustion; the
    * mutant's manifest is still well formed, so the round-trip law holds.
    */
  private def killed(mutant: Graph => Either[CodecError, StoredGraph]): Boolean =
    val ruleSet    = ManifestLaws.verifiedResolution(graphs, mutant, decoders, reproduces)
    val wellFormed = outcome(
      ruleSet,
      "the stored manifest bytes decode to the manifest and are its canonical form"
    ) match
      case Test.Passed | Test.Proved(_) => true
      case _                            => false
    val falsified =
      outcome(ruleSet, "a written graph resolves by address, reading each artifact once") match
        case Test.Failed(_, _) | Test.PropException(_, _, _) => true
        case _                                               => false
    wellFormed && falsified

  /** A writer that stores a changed manifest, canonically encoded, beside the
    * kept entries.
    */
  private def mutant(
      change: StoredGraph => Either[ManifestError, ScientificManifest],
      keep: StoredGraph => Map[ArtifactName, IArray[Byte]] = _.entries
  ) =
    (g: Graph) =>
      write(g).flatMap(s =>
        change(s).left
          .map(CodecError.Manifest.apply)
          .flatMap(m => StoredGraph.canonical(m, keep(s)))
      )

  private def entry(graph: StoredGraph, value: String): ManifestEntry =
    get(graph.manifest.entry(name(value)).toRight(value))

  private def withEntry(graph: StoredGraph, value: String)(
      change: ManifestEntry => Either[ManifestError, ManifestEntry]
  ): Either[ManifestError, ScientificManifest] = for
    changed  <- change(entry(graph, value))
    manifest <- ScientificManifest.of(
      graph.manifest.entries.map(e => if e.name == changed.name then changed else e),
      graph.manifest.relations
    )
  yield manifest

  test(
    "the published laws kill a dropped entry, an altered digest and a relation to the wrong artifact"
  ) {
    // The ledger and its relation vanish: the manifest stays well formed but no longer describes the graph.
    val droppedEntry = mutant(
      graph =>
        ScientificManifest.of(
          graph.manifest.entries.filterNot(_.name == name("ledger")),
          graph.manifest.relations.filterNot(_.endpoints.exists(_._2 == name("ledger")))
        ),
      _.entries - name("ledger")
    )
    assert(killed(droppedEntry))

    // The result entry declares the digest of other bytes.
    val alteredDigest = mutant(graph =>
      withEntry(graph, "result")(e =>
        ManifestEntry.of(
          e.name,
          e.role,
          e.schema,
          e.media,
          e.length,
          ByteDigest.sha256(IArray.empty[Byte]),
          e.identity,
          e.layout
        )
      )
    )
    assert(killed(alteredDigest))

    // The result claims to be computed on the temporal base study.
    val wrongRelation = mutant(graph =>
      ScientificManifest.of(
        graph.manifest.entries,
        graph.manifest.relations.map {
          case ManifestRelation.ResultOf(result, plan, _) =>
            ManifestRelation.ResultOf(result, plan, name("base"))
          case other => other
        }
      )
    )
    assert(killed(wrongRelation))

    // The input entry declares the base study's semantic identity.
    val swappedIdentity = mutant(graph =>
      val base = entry(graph, "base")
      withEntry(graph, "input")(e =>
        ManifestEntry.of(
          e.name,
          e.role,
          e.schema,
          e.media,
          e.length,
          e.sha256,
          base.identity,
          e.layout
        )
      )
    )
    assert(killed(swappedIdentity))
  }

  test(
    "a mutant whose writer is lawful is not killed: the criterion does not fire spuriously"
  ) {
    assert(!killed(write))
  }
