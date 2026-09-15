# eyes4s

*A composable scientific foundation for eye-movement analysis in Scala 3.*

The aim is to subsume eyesim’s basic analysis workflows through a clear direct API, then support
new methods and a separate analysis application through stable, typed contracts. Read the
[vision and mission](docs/VISION.md), [development plan](docs/DEVELOPMENT_PLAN.md), and
[eyesim capability baseline](docs/EYESIM_CAPABILITIES.md).

A gaze record is a timed trajectory through a known geometry, and it has a shadow: the measure that
trajectory induces on the stimulus. Eye-movement statistics live on one side or the other of that
duality, and knowing which side is half the design.

> **eyes4s makes the conventions that eye-movement analysis leaves in the analyst's head — which
> screen, which origin, which unit, which clock, whether this map is normalised — into types the
> compiler checks.**

Most libraries in this space represent a fixation as a row of floats. The screen, viewing distance,
y-axis direction, clock domain, and normalisation state remain conventions outside the value. The
result is a tax paid in silent unit errors, y-flips, and comparisons between incommensurable maps.

```scala
for
  display <- Frame.screen("bench", width = 1280, height = 1024)
  visual  <- Frame.angular("visual-field", width = 47.7, height = 28.0)
  viewing <- Viewing.of(
    distance = Length.mm(600),
    screenWidth = Length.mm(530),
    screenHeight = Length.mm(300)
  )
yield Viewing.angularWarp(viewing, display, visual)
// Either[GeometryError, Warp[Px, Deg]]
```

That warp can transform `Gaze[Px]` into `Gaze[Deg]`; it cannot be applied backwards or to data from
another frame. Invalid dimensions are construction errors, not exceptional control flow.

Typed does not have to mean ceremonial. Common scientific designs have a thin vocabulary over the
same inspectable algebra:

```scala
final case class TrialKey(subject: String, image: String)

val subject = Projection.named[TrialKey, String]("subject")(_.subject)
val image   = Projection.named[TrialKey, String]("image")(_.image)

val controls =
  Pairing
    .within[TrialKey]
    .sameOn(subject)
    .differentOn(image)
    .excludingSelf
    .bottomK(50, Seed(2026L), SampleId("controls"))
// Either[PairingError, PairDesign.WithinDirected[TrialKey]]
```

The call reads as the design, while the result remains an inspectable `PairDesign` made from named
relations rather than an opaque predicate. Impossible orientation and sampling combinations are
absent from the API; invalid raw configuration is reported as data.

## Status

**Pre-alpha, under active implementation.** The typed kernel, gaze core, detectors, surfaces,
comparison measures, relational design algebra, deterministic RNG, and published law suites have
executable implementations and tests. AOI, delimited I/O, and the portable EyeLink ASC path
also have implementations. Matched/control contrasts now retain signed results, compatibility
checks, and per-key evidence. The fixation-study path has CSV admission, typed saved plans,
versioned codecs, multiscale execution, and tidy exports. General detection/AOI/temporal plans and
complete baseline coverage remain in progress. APIs can change before the
first release. EyeLink ASC parser evidence is not yet vendor or real-device certification; see the
[support and import guide](docs/formats/eyelink-asc.md).

- [`eyes4s.md`](eyes4s.md) — architecture specification: the thesis, the design pillars, the five
  layers, and the traps being designed against.
- [`PRD.md`](PRD.md) — product requirements: numbered requirements, error model, verification
  requirements, versioned roadmap, and acceptance criteria.
- `.mote/` — the tracked plan and decision record. Source comments cite resolved decisions as
  `bead q-<name>`, following house convention.

```sh
mote board     # overview
mote ready     # what is actionable now
mote show k-warp
```

## Analyze an existing fixation table

Start with the [fixation study guide](docs/FIXATION_STUDIES.md) and its
[compiled Scala example](docs/examples/StudyGuide.scala). It takes a CSV table through explicit
geometry and timing, matched/control comparison, a saved/reloaded plan, and an R-readable result.
Then see [saved studies](docs/SAVED_STUDIES.md) for inspection and versioning, or
[extending studies](docs/EXTENDING_STUDIES.md) to add a method in an independent consumer.

## Scope

**v1.0** completes the baseline from raw samples or existing fixations through scanpaths,
occupancy measures, comparison, and contrasts, with usable APIs, saved plans, and extension proof. Modules `kernel`, `core`, `detect`, `surface`, `aoi`, `compare`,
`design`, `plan`, `codec`, `laws`, `fs2`, `io`.

**Subsequent modules** extend the foundation with reading measures, additional saliency metrics,
adaptive detection, vendor formats, pupillometry, and advanced modeling. Some additional methods,
including CRQA, already have code; implementation presence does not establish release readiness.
The separate analysis UI is outside this repository.

Deliberately **not** in scope: a statistics package (no mixed models — results are exported), a
plotting library (specifications only), vendor SDK bindings, and saliency-model training.

## Limitations to know before you start

- **Binary `.edf` is not supported.** SR Research's format requires the proprietary `edfapi`. Run
  `edf2asc` first and ingest the `.asc`.
- **Platforms are JVM and Scala.js.** Scala Native is deferred post-1.0; dependencies are kept
  Native-eligible so adding the axis stays a build change.
- **eyesim baseline coverage is a goal, not a completed claim.** The
  [capability map](docs/EYESIM_CAPABILITIES.md) distinguishes existing primitives from complete
  workflows. Verified shared conventions require agreement; confirmed defects require explicit
  divergence tests. The [parity report](PARITY.md) records the bounded evidence; broader migration coverage remains open.

## Relationship to eyesim

[`eyesim`](https://github.com/bbuchsbaum/eyesim) supplies the basic capability baseline: fixation
representation, density and entropy, similarity, template and repetition designs, and multiscale
and temporal analysis. eyes4s aims to make these workflows more coherent while adding raw-sample
analysis and contracts for future modules. It preserves scientific capabilities, with verified
conventions and documented differences, rather than reproducing R function signatures.

## Building

Requires JDK 17+ and sbt.

```sh
sbt compileAll
sbt testAll
sbt checkBoundaries
```

## Licence

Apache-2.0. See [LICENSE](LICENSE).
