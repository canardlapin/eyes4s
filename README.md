# eyes4s

*A typed, lawful core for eye-movement analysis in Scala 3.*

A gaze record is a timed trajectory through a known geometry, and it has a shadow: the measure that
trajectory induces on the stimulus. Eye-movement statistics live on one side or the other of that
duality, and knowing which side is half the design.

> **eyes4s makes the conventions that eye-movement analysis leaves in the analyst's head — which
> screen, which origin, which unit, which clock, whether this map is normalised — into types the
> compiler checks.**

Every library in this space represents a fixation as a row of floats. None carries the screen, the
viewing distance, the y-axis direction, the clock domain, or the normalisation state in the value.
The result is a tax paid in silent unit errors, y-flips, and comparisons between incommensurable
maps.

```scala
val screen = Frame.screen("bench", 1280, 1024)              // Frame[Px], y-down
val fovea  = Frame.angular("fovea", Extent(34.0, 27.0))     // Frame[Deg]
val toDeg  = Warp.tangent(screen, fovea, Viewing(mm(600), PhysicalScreen(mm(530), mm(300))))

val pipeline: Machine[Sample[Deg], Event[Deg]] =
  Filter.deblink[Deg](maxGap = ms(75), pad = ms(20))
    .andThen(Detector.ivt(threshold = degPerSec(30), minDuration = ms(60)))

for
  inDeg <- recording.warp(toDeg)          // Recording[Px] => Recording[Deg]
  events = pipeline.runAll(inDeg.samples)
  sp    <- Scanpath.fromEvents(fovea, inDeg.clock, events)
  dwell <- aois.dwell(sp)
yield dwell
```

`pipeline` consumes `Sample[Deg]`, not `Sample[?]`, so the warp is not a stylistic choice — a 30°/s
threshold cannot reach pixel data, and producing `toDeg` requires stating the viewing geometry.

## Status

**Pre-alpha. Nothing is implemented yet.** This repository currently holds the design and the plan.

- [`eyes4s.md`](eyes4s.md) — architecture specification: the thesis, the design pillars, the five
  layers, and the traps being designed against.
- [`PRD.md`](PRD.md) — product requirements: numbered requirements, error model, verification
  requirements, versioned roadmap, and acceptance criteria.
- `.mote/` — the tracked plan. 99 open work items, 10 resolved decisions with their rationale.
  Source comments cite decisions as `bead q-<name>`, following house convention.

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
sbt checkModuleBoundaries
```

## Licence

Apache-2.0. See [LICENSE](LICENSE).
