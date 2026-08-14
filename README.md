# eyes4s

*A typed, lawful core for eye-movement analysis in Scala 3.*

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
executable implementations and tests. AOI, analysis-plan, codec, and file-I/O modules remain
scaffolds; APIs can still change before the first release.

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

## Scope

**v1.0** is the thesis core: raw samples → event detection → scanpaths → occupancy measures →
lawful comparison → contrast. Modules `kernel`, `core`, `detect`, `surface`, `aoi`, `compare`,
`design`, `plan`, `codec`, `laws`, `fs2`, `io`.

**v1.1** is the first expansion beyond what `eyesim` can express: reading measures, the saliency
metric family, BIDS eye-tracking ingest, and CRQA.

Deliberately **not** in scope: a statistics package (no mixed models — results are exported), a
plotting library (specifications only), vendor SDK bindings, and saliency-model training.

## Limitations to know before you start

- **Binary `.edf` is not supported.** SR Research's format requires the proprietary `edfapi`. Run
  `edf2asc` first and ingest the `.asc`.
- **Platforms are JVM and Scala.js.** Scala Native is deferred post-1.0; dependencies are kept
  Native-eligible so adding the axis stays a build change.
- **Parity with `eyesim` is advisory, not guaranteed.** Several `eyesim` behaviours are defects, and
  gating on agreement would encode them as requirements. Divergences are documented in `PARITY.md`
  with their cause and a statement of which implementation is correct.

## Relationship to eyesim

[`eyesim`](https://github.com/bbuchsbaum/eyesim) is the R package this work grew out of, and a full
read of it is the primary evidence base for the requirements. `eyes4s` is not a port: it starts at
raw samples rather than fixations, carries geometry in types, and makes comparison heterogeneous —
three changes that put microsaccades, data quality, pupillometry, reading measures, saliency
benchmarking, and statistical mapping inside one library instead of five.

## Building

Requires JDK 17+ and sbt.

```sh
sbt compileAll
sbt testAll
sbt checkBoundaries
```

## Licence

Apache-2.0. See [LICENSE](LICENSE).
