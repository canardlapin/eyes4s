# eyes4s

*A typed, lawful core for eye-movement analysis in Scala 3.*

## Vision

`eyesim` asks one question well: **are two fixation patterns similar?** `eyes4s` should answer that
question *and be the substrate underneath it* — a core for eye-movement analysis, event detection,
attention mapping, AOI analysis, scanpath comparison, and gaze-contingent tracking, general enough
to serve cognitive psychology rather than one paradigm.

The thesis is small enough to state in one sentence:

> **A gaze record is a timed trajectory through a known geometry, and it has a shadow: the measure
> that trajectory induces on the stimulus. Eye-movement statistics live on one side or the other of
> that duality, and knowing which side is half the design.**

The forgetful map is explicit and lossy:

```scala
def occupancy[U <: Unit2D](sp: Scanpath[U], w: Weight): PointMeasure[U]   // discards order
```

**On the occupancy side**, where order has been forgotten, one operation generates the literature.
Dwell time is the measure of a region. A heat map is the measure convolved with a kernel. Entropy is
a functional of it. NSS and AUC are integrals of a saliency map against it. Earth-mover's distance is
a Wasserstein metric between two of them. These stop being a dozen unrelated functions and become one
`integrate` plus a library of integrands.

**On the trajectory side**, order is the content, and no integral will recover it: MultiMatch,
ScanMatch, DTW, CRQA, scanpath length, transition matrices, run counts, first-entry times, the
ambient/focal K coefficient. These are alignment and sequence problems, and they get their own
factored kernel (Layer 4) rather than being forced through a measure they would destroy.

The design's job is to make the two sides distinct types, make the map between them explicit, and
refuse to let a statistic be computed on the side that cannot support it.

The one-sentence pitch, in the house idiom:

> **What Cats did for effect composition, eyes4s does for gaze: it turns the conventions that
> eye-movement analysis leaves in the analyst's head — which screen, which origin, which unit,
> which clock, whether this map is normalised — into types the compiler checks.**

### The evidence that this is worth doing

`eyesim` is a good package written by a careful person. That is precisely why its failure modes are
worth cataloguing: they are not sloppiness, they are the *predictable* consequence of representing a
physical measurement as an untyped data frame addressed by string column names. A full read of its
5,143 lines turned up, among others:

| Failure | Where | Root cause |
|---|---|---|
| The flagship vignette's `template_similarity(enc, ret, match_on = "image")` compares **every** participant's retrieval map against **participant s1's** encoding map, because `match()` takes the first hit. The vignette demonstrates the `paste(participant, image)` workaround two sections later without noting the earlier call needed it. | `vignettes/eyesim.Rmd` §c | The join key is a *string naming a column*, not a value of a key type. |
| The same `sigma` produces a **4× different bandwidth** depending on whether `ks` is installed, because `MASS`-style backends internally do `h <- h/4`. A `message()` announces the fallback mid-computation. | `similarity.R:1337` vs `:1364` | "Bandwidth" is a bare `Double` with a per-backend convention. |
| `+` on two density maps is secretly `(z1 + z2)/2`; `/` is secretly `log(z1/z2)`; all three `Ops` drop `sigma` and return objects still tagged `eye_density`, so a **signed** difference map is accepted by `fixation_entropy` as if it were a probability mass. | `similarity.R:1411` | One type for three different mathematical objects. |
| `eye_density(..., weights = )` is documented, is one of seven positional parameters on the generic, and is **never read** by the method. | `similarity.R:1185` | No compiler to notice an unused parameter. |
| `estimate_scale` fits its scaling on columns `1:2` of a `fixation_group` — which are `index` and `x`, not `x` and `y`. | `expansion.R:30` | Positional column indexing on a schema that varies by construction path. |
| Three permutation baselines with three different semantics (remove-self-then-subsample vs subsample-then-remove-self; first-occurrence vs all-occurrences), so the realised `n_perm` is nondeterministic and must be returned as a column. | `similarity.R:196`, `:268`, `:976` | The baseline is control flow, not a value. |
| `eye_table` computes the complete coordinate frame — clip bounds, x-direction, y-direction, whether coordinates were relativised — and persists **only the centroid** in an attribute that `[.eye_table` then drops. | `eye_frame.R:33` | Geometry is an argument, not a member. |
| `density_matrix` is exported, documented, has an example, and has **zero methods**. The operation it names is re-implemented three times as private helpers with divergent NA and ragged-length behaviour. | `all_generic.R` | Nothing checks that a declared abstraction is inhabited. |
| Nine different parameter names for "the name of the list-column holding my objects": `fixvar`, `refvar`, `sourcevar`, `template_var`, `source_var`, `density_var`, `result_name`, `outcol`, `outvar`. | throughout | The container is a data frame; the payload is addressed by string. |

In the design proposed below, six of these nine are type errors and several are unrepresentable
rather than merely detected. Two are honest exceptions worth naming up front: the ignored `weights=`
parameter becomes an unused-parameter warning rather than an error, and the `match_on` bug becomes a
*visible, deliberate one-liner* (`Pairing.matchedOn(_.image)`) rather than something the compiler
forbids — you can still ask for the wrong join, but you can no longer ask for it by accident.

---

## Why Scala, why Typelevel, why now

### The landscape

| Tool | Language | What it is |
|---|---|---|
| `eyesim`, `eyetrackingR`, `gazeR`, `saccades`, `eyelinker`, `popEye` | R | data-frame pipelines; each owns a slice (similarity, growth curves, detection, IO) |
| `pymovements`, `PyGaze`, `REMoDNaV`, `I2MC`, `multimatch-gaze`, `pysaliency` | Python | NumPy arrays + pandas; the strongest detection and benchmarking implementations live here |
| Titta, iohub/PsychoPy, Tobii/EyeLink SDKs | Python/MATLAB | acquisition, not analysis |

Every one of them represents a fixation as a row of floats. None carries the screen, the viewing
distance, the y-axis direction, or the clock domain in the value. The result is a literature-wide
tax paid in silent unit errors, y-flips, and comparisons between incommensurable maps.

### The substrate

| Need | Answer |
|---|---|
| Monoids, groups, orders, `NonEmptyVector`, `Validated` | `cats-core`, `cats-kernel` |
| Lawful abstractions + law testing | Cats, Discipline, ScalaCheck, munit |
| Dense/sparse linear algebra, SVD, solvers | **`gale`** (sibling) |
| Matrix-free operators, Krylov solves | **`linop4s`** (sibling) |
| Transition graphs, connected components | **`graph4s`** (sibling) |
| Typed tabular results | **`frame4s`** (sibling) |
| Grammar of graphics, JVM + JS | **`intaglio`** (sibling) |
| Streaming acquisition and parsing | FS2, Cats Effect |
| Cross-platform (JVM / JS / Native) | sbt-typelevel |
| **Geometry, units, gaze events, occupancy measures** | **eyes4s defines these** |

The sibling ecosystem covers everything except the domain itself. That is the right amount of
greenfield.

### Why now

1. **BIDS now has an eye-tracking specification.** There is finally a standard ingestion target
   worth typing against, rather than N vendor formats and a shrug.
2. **Mobile and VR eye tracking make frame composition unavoidable.** Mapping gaze from a head-worn
   camera onto a stimulus surface is a *composition of time-varying warps*. Every library that
   models coordinates as bare `(x, y)` has to solve this ad hoc; a library whose frames form a
   category gets it for free.
3. **Saliency-model benchmarking is a mature literature** (AUC-Judd, shuffled AUC, NSS, CC, SIM,
   KLD, Information Gain) built entirely on one primitive — *a map compared against a point set* —
   which `eyesim` lacks entirely. That primitive is a two-line consequence of the design below.
4. **Gaze-contingent paradigms need online detection.** A detector written as a pure state machine
   runs identically offline over a `Vector` and online over an `fs2.Stream`. No other library in
   this space offers one implementation for both.
5. **Cross-compilation is a genuine asset here.** The same law-tested detector that runs on the JVM
   for analysis runs in the browser under Scala.js for a web-based study platform. Neither Python
   nor R can make that claim.

---

## Non-goals

eyes4s is not:

- a general time-series or signal-processing library (it consumes one);
- a statistics package — no mixed models, no GLMM; it produces tidy results and hands off;
- a plotting library (that is `intaglio`; eyes4s exposes plot *specifications*);
- a vendor SDK binding or acquisition driver in core;
- a saliency-model *training* framework (evaluation, yes);
- a tensor library;
- a package that encodes grid dimensions at the type level;
- a package that pretends every comparison it offers is a metric.

---

## Design pillars

1. **Geometry is a value, carried by construction.** A `Scanpath` knows its frame. There is no
   function in the library that takes a screen size as an argument you might forget.
2. **Units are static; frame identity is nominal-runtime.** `Px` vs `Deg` is a compile-time
   distinction because it is knowable at compile time and it is the single most common error.
   *Which* screen or *which* stimulus is a runtime value with nominal identity, because you cannot
   know at compile time which of 200 images a trial belongs to. This is the same split `linop4s`
   made for `Space`, for the same reason.
3. **Frames and clocks are categories; conversion is a morphism you must supply.** You cannot obtain
   degrees of visual angle without producing a `Viewing`. You cannot align an eye-tracker timestamp
   to a stimulus timestamp without producing a `Sync`.
4. **Parse, don't validate.** `Recording`, `Scanpath`, `Mass` have private constructors and
   `Either`-returning smart constructors. Monotone timestamps, non-negative durations, in-frame
   coordinates and unit mass are properties of the type, not of the caller's discipline.
5. **Trajectory and occupancy are dual.** One `integrate`; many statistics.
6. **A measure declares its own result type, its own scale, and its own laws.** `MultiMatch` returns
   a `MultiMatchScore` with five fields, not a named vector of six that the caller must reshape.
7. **One detector definition, two runtimes.** Pure Mealy machines; `runAll` in core, `toPipe` in the
   FS2 module.
8. **Designs, pairings, and baselines are data.** The permutation baseline is not a code path; it is
   a different `Pairing` passed to the same function.
9. **Everything lawful is law-tested, and every convention is named in a type.** Half-open windows,
   column-major flattening, sigma-is-a-standard-deviation: each is a named type or a named policy,
   never a comment.

---

## Core abstractions

### Layer 0 — quantities, time, and clocks

```scala
package eyes4s

opaque type Instant = Long           // exact device time in microseconds; no float drift
opaque type Span    = Long           // a signed extent on some timeline
opaque type Hz      = Double

object Instant:
  given Order[Instant]
  extension (a: Instant)
    def -(b: Instant): Span
    def +(s: Span): Instant

object Span:
  given Order[Span]
  given CommutativeGroup[Span]
  extension (s: Span) def toSeconds: Double

final case class ClockId(name: String)

/** ABSOLUTE: a half-open interval [onset, offset) on a named timeline.
  * The offset is STORED, never implied. Produced by events; rarely hand-built. */
final case class Interval(clock: ClockId, onset: Instant, offset: Instant):
  def duration: Span
  def contains(t: Instant): Boolean      // onset <= t < offset
  def overlaps(that: Interval): Either[ClockMismatch, Boolean]

/** RELATIVE: an analysis window, two Spans from a named anchor. Carries no clock,
  * because it has none. `Window(ms(0), ms(3000))` was never an Interval. */
final case class Window(from: Span, until: Span)
enum Anchor: case TrialOnset, FirstFixation, StimulusEvent(name: String)

/** How a window selects events that straddle its edges. */
enum Overlap:
  case OnsetInside      // eyesim's implicit rule, now named
  case FullyContained
  case AnyIntersection
```

`Micros` is exact and totally ordered; a `Span` is a `CommutativeGroup`. `eyesim` has no `offset`
column anywhere in the package and filters windows on `onset` alone at four inlined call sites with
three different validation regimes. Here the convention has a name and one implementation.

Clocks are nominal-runtime values, exactly like frames, and for the same reason — a session has an
eye-tracker clock, a stimulus-presentation clock, and a system clock, and they drift:

```scala
final case class Sync(from: ClockId, to: ClockId, offsetMicros: Long, drift: Double):
  def apply(t: Instant): Instant
  def inverse: Sync
```

There is no static tag on `Instant` because, unlike space, time has one unit. The distinction that
needs enforcing is *which timeline*, and that is a value — carried by `Recording`, `Scanpath`, and
`Interval`.

The `Interval` / `Window` split is what makes that affordable. Absolute intervals come from events
and inherit their clock, so the `ClockId` field costs nothing at the call site; relative windows —
the thing analysts actually write by hand — need no clock at all. `eyesim` has only the absolute
form, which is why its every window filter is expressed as raw onsets and why a stimulus-clock
timestamp can silently enter a tracker-clock comparison.

The limit anticipated here — that `Instant` and `Span`, both opaque over `Long`, would be mutually
assignable inside the library — turned out to be avoidable. Defining each in its **own compilation
unit** means neither is transparent where the other is in scope, so the separation holds everywhere,
including in the library's own implementation. `TimeSuite` asserts both directions with
`typeCheckErrors`.

### Layer 1 — geometry: units, frames, warps

```scala
sealed trait Unit2D
object Unit2D:
  sealed trait Px   extends Unit2D    // device / screen pixels
  sealed trait Deg  extends Unit2D    // degrees of visual angle
  sealed trait Norm extends Unit2D    // normalised stimulus coordinates in [0,1]²
  sealed trait Mm   extends Unit2D    // physical

final case class Pt(x: Double, y: Double)     // bare pair; frames live on collections
final case class Extent(w: Double, h: Double)
enum YAxis:  case Down, Up                    // device convention vs mathematical

final case class FrameId(name: String)

/** A planar coordinate frame: identity, extent, and axis orientation, in a static unit. */
final class Frame[U <: Unit2D] private (
    val id: FrameId,
    val extent: Extent,
    val yAxis:  YAxis
):
  def contains(p: Pt): Boolean
  def diagonal: Double                        // computed ONCE, not five times as in multimatch.R
  def centre: Pt

object Frame:
  def screen(name: String, w: Int, h: Int, yAxis: YAxis = YAxis.Down): Frame[Unit2D.Px]
  def unitSquare(name: String): Frame[Unit2D.Norm]
  def angular(name: String, extent: Extent): Frame[Unit2D.Deg]
```

Frames put on collections, never on points: a whole trial shares a frame, exactly as a `NeuroVol`
shares a `NeuroSpace`. Per-point frames would be both slower and a lie about the data.

Conversions retain structure, as `linop4s`'s `LinearMap` does — they are an inspectable ADT, not an
opaque closure:

```scala
sealed trait Warp[A <: Unit2D, B <: Unit2D]:
  def from: Frame[A]
  def to:   Frame[B]
  def apply(p: Pt): Pt
  def inverse: Option[Warp[B, A]]
  def render: String                    // "screen[px] --tangent(600mm)--> fovea[deg]"

object Warp:
  final case class Id[A <: Unit2D](frame: Frame[A]) extends Warp[A, A]:
    def from = frame; def to = frame
  final case class Affine[A <: Unit2D, B <: Unit2D] private[eyes4s] (
      from: Frame[A], to: Frame[B], m: Mat3)                            extends Warp[A, B]
  /** px <-> deg. NONLINEAR (tangent / arctangent). `sense` selects the direction, so the
    * pairing is genuinely invertible rather than a one-way door. */
  final case class Tangent[A <: Unit2D, B <: Unit2D] private[eyes4s] (
      from: Frame[A], to: Frame[B], viewing: Viewing, sense: Sense)     extends Warp[A, B]
  /** Planar surface mapping for head-worn cameras / marker-tracked stimuli. */
  final case class Homography[A <: Unit2D, B <: Unit2D] private[eyes4s] (
      from: Frame[A], to: Frame[B], h: Mat3)                            extends Warp[A, B]
  final case class Then[A <: Unit2D, B <: Unit2D, C <: Unit2D] private[eyes4s] (
      f: Warp[A, B], g: Warp[B, C])                                     extends Warp[A, C]

  enum Sense: case Forward, Inverse

  // Smart constructors are the ONLY public way in; `Then` cannot be built unchecked.
  def tangent(screen: Frame[Unit2D.Px], fovea: Frame[Unit2D.Deg], v: Viewing)
      : Warp[Unit2D.Px, Unit2D.Deg]
  def affine[A <: Unit2D, B <: Unit2D](from: Frame[A], to: Frame[B], m: Mat3)
      : Either[SingularWarp, Warp[A, B]]

  extension [A <: Unit2D, B <: Unit2D](f: Warp[A, B])
    def andThen[C <: Unit2D](g: Warp[B, C]): Either[FrameMismatch, Warp[A, C]]

final case class Viewing(distance: Length, screen: PhysicalScreen)
```

Every case has a private constructor; `Then` is reachable only through `andThen`, which checks that
`f.to.id == g.from.id`. A public `Then` would let a caller assemble an unchecked composition and
defeat the discipline the type exists to enforce.

**Frame identity is checked wherever two frame-carrying values meet.** The unit parameter `U` proves
only that both sides are in degrees — not that they are in the *same* degrees. So every binary
operation across the library takes the same shape:

```scala
def massIn[U <: Unit2D](m: PointMeasure[U], r: Region[U]): Either[FrameMismatch, Double]
def dwell [U <: Unit2D, K](s: AoiSet[U, K], sp: Scanpath[U]): Either[FrameMismatch, Map[K, Span]]
def smooth[U <: Unit2D](m: PointMeasure[U], g: Grid[U]): Either[EstimateError, Intensity[U]]
```

Without this, a `Region` defined on stimulus A and a `Scanpath` recorded over stimulus B would
combine silently — precisely the bug class the design exists to eliminate. Ergonomic relief comes from a checked
*container*, not a capability: `Session[U]` validates frame membership on insertion — returning
`Either` there — and its accessors are total, because membership was proven on the way in. A
capability value would be forgeable, since one obtained for frame A can be applied to objects from
frame B; a container's contents were validated on entry. `Session` is also the project object the
application layer needs.

Composition is a **partial category**: associativity and identity hold on the subcategory where
frame identities agree, and `andThen` returns `Either[FrameMismatch, _]` off it. The law suite
checks the laws on that subcategory and checks that `andThen` refuses everywhere else. This is
honest, and it is the only correct statement — a total `Category` instance would require pretending
that `screen_A --> stimulus_B` composes with `stimulus_C --> deg`.

**Time-varying frames** fall straight out and cover dynamic stimuli, scrolling pages, video, and
mobile eye tracking:

```scala
final case class Moving[A <: Unit2D, B <: Unit2D](
    segments: Vector[(Interval, Warp[A, B])],
    interp:   Interp                        // Constant | Linear
):
  def at(t: Instant): Option[Warp[A, B]]
```

A dynamic AOI is then just a `Region` in a moving frame. No special case, no new machinery.

### Layer 2 — trajectory: samples, events, scanpaths

No `NA`. Missing data is an ADT:

```scala
enum Gaze[U <: Unit2D]:
  case Tracked  (p: Pt, pupil: Option[Double]) extends Gaze[U]
  case Blink    ()                             extends Gaze[U]
  case Lost     ()                             extends Gaze[U]
  case OffScreen(p: Pt)                        extends Gaze[U]

/** Unit-parameterised, so a sample in pixels is a different type from a sample in degrees. */
final case class Sample[U <: Unit2D](t: Instant, gaze: Gaze[U])
enum Eye: case Left, Right, Cyclopean
enum Rate:
  case Fixed(hz: Hz)
  case Irregular                     // webcam / Pupil Labs / variable-rate devices

final class Recording[U <: Unit2D] private (
    val frame:   Frame[U],
    val clock:   ClockId,
    val rate:    Rate,
    val eye:     Eye,
    val samples: IArray[Sample[U]]
):
  def warp[V <: Unit2D](f: Warp[U, V]): Either[FrameMismatch, Recording[V]]

object Recording:
  def of[U <: Unit2D](frame: Frame[U], clock: ClockId, rate: Rate, eye: Eye,
                      samples: IArray[Sample[U]]): Either[RecordingError, Recording[U]]

/** Binocular data is a SEPARATE type, so the monocular path carries no tax and
  * disparity stays recoverable. Filtering two recordings independently destroys
  * the sample-level pairing that vergence depends on. */
final class BinocularRecording[U <: Unit2D] private (...):
  def left: Recording[U]
  def right: Recording[U]
  def cyclopean(f: Fusion): Recording[U]
  def vergence: Vector[(Instant, Angle)]
```

The unit parameter on `Sample` is what makes the detector claim true. Without it,
`Detector.ivt(degPerSec(30))` would happily consume a stream of pixel samples and the only thing
"typed" would be the literal. See Layer 2's detector section.

Events carry a real interval and the frame's unit, so neither the n-vs-(n−1) confusion nor a
unit mix-up can arise:

```scala
sealed trait Event[U <: Unit2D]:  def span: Interval
final case class Fixation[U <: Unit2D](span: Interval, centre: Pt, dispersion: Double, nSamples: Int)
    extends Event[U]
final case class Saccade[U <: Unit2D](span: Interval, from: Pt, to: Pt, peakVelocity: Velocity[U])
    extends Event[U]
final case class Blink[U <: Unit2D](span: Interval)                extends Event[U]
final case class Pursuit[U <: Unit2D](span: Interval, path: IArray[Pt]) extends Event[U]

final class Scanpath[U <: Unit2D] private (
    val frame:     Frame[U],
    val clock:     ClockId,
    val fixations: NonEmptyVector[Fixation[U]]
):
  def n: Int
  /** Exactly n - 1 saccades. No padded zero row. */
  def saccades: Vector[Saccade[U]]
  def window(w: Interval, policy: Overlap): Option[Scanpath[U]]
  def warp[V <: Unit2D](f: Warp[U, V]): Either[FrameMismatch, Scanpath[V]]

object Scanpath:
  def fromEvents[U <: Unit2D](frame: Frame[U], clock: ClockId, es: Vector[Event[U]])
      : Either[ScanpathError, Scanpath[U]]
```

`eyesim`'s `scanpath` class appends `lenx, leny, rho, theta` to the fixation table and pads the last
row with zeros, which every consumer must then remember to drop (`multi_match` does
`sacx <- x[1:(nrow(x)-1),]`). Here the arity difference is in the types.

**Detectors are pure Mealy machines.** This is the pillar that makes "tracking" a first-class use
case rather than a separate library:

```scala
trait Detector[S, -I, +O]:
  def init: S
  def step(s: S, i: I): (S, Chunk[O])
  def flush(s: S): Chunk[O]

/** Existential wrapper hiding the state type, so composition has a well-kinded Category. */
sealed trait Machine[-I, +O]:
  type S
  val detector: Detector[S, I, O]

object Machine:
  def apply[St, I, O](d: Detector[St, I, O]): Machine[I, O]
  extension [I, O](m: Machine[I, O])
    def runAll(in: Iterable[I]): Vector[O]              // core, pure; step* then flush
    def andThen[P](n: Machine[O, P]): Machine[I, P]
  given Category[Machine] = ...
```

`Category[Machine]` is law-tested up to **observational equality** — the composite state types are
`((S, T), U)` versus `(S, (T, U))`, which are isomorphic but not `==`, so the law is stated over the
output sequences produced by `runAll`, not over states. This is the only correct formulation and the
law suite says so.

`eyes4s-fs2` adds `def toPipe[F[_]]: fs2.Pipe[F, I, O]`. The same law-tested I-VT implementation
runs over a `Vector` offline and over a live 1000 Hz stream in a gaze-contingent experiment. One
caveat stated rather than buried: `runAll` and `toPipe` agree on *finite* input, because `runAll`
always calls `flush`. On an unbounded live stream `flush` never runs, so the final in-progress event
is not emitted until the stream terminates — correct behaviour, but it means "identical output"
holds for finite streams only.

Shipped detectors and filters, each with the parameters that define it *in its own units*:

```scala
Detector.ivt(threshold: Velocity[Unit2D.Deg], minDuration: Span)
    : Machine[Sample[Unit2D.Deg], Event[Unit2D.Deg]]
Detector.idt(extent: Extent, minDuration: Span)                 // a bounding box, NOT a Sigma
Detector.engbertKliegl(lambda: Double, minDuration: Span)       // microsaccades
Detector.nystromHolmqvist(...)                                  // adaptive threshold
Detector.i2mc(...)
Filter.savitzkyGolay[U <: Unit2D](window: Int, order: Int): Machine[Sample[U], Sample[U]]
Filter.median[U <: Unit2D](window: Int)
Filter.deblink[U <: Unit2D](maxGap: Span, pad: Span)
Merge.adjacentFixations[U <: Unit2D](maxGap: Span, maxAngle: Angle)
```

`Detector.ivt` consumes `Sample[Deg]`, not `Sample[?]`. An I-VT threshold of 30°/s is meaningless in
pixels, and because the *input* is unit-parameterised — not merely the threshold literal — you must
warp the recording to `Deg` first, which means you must have produced a `Viewing`. Note also that
I-DT's dispersion is a bounding-box extent, not a standard deviation, so it takes an `Extent` rather
than punning on `Sigma`.

### Layer 3 — occupancy: measures, grids, surfaces, regions

This is the layer that unifies everything `eyesim` does piecewise.

```scala
/** A finite discrete measure on the plane: weighted points in a frame. */
final class PointMeasure[U <: Unit2D] private (
    val frame:  Frame[U],
    val points: IArray[Pt],
    val mass:   IArray[Double]
):
  def total: Double
  def normalised: PointMeasure[U]
  def integrate(f: Pt => Double): Double
  def massIn(r: Region[U]): Double

object PointMeasure:
  enum Weight: case Uniform, Duration, Custom(f: Fixation => Double)
  def fromScanpath[U <: Unit2D](sp: Scanpath[U], w: Weight): PointMeasure[U]
  def fromSamples [U <: Unit2D](r: Recording[U]): PointMeasure[U]     // each sample carries 1/rate
```

`integrate` generates this layer — the *occupancy* half of the duality, where order has been
forgotten. Dwell time is `massIn`. NSS is `integrate` against a z-scored surface. Template sampling
is `integrate` against a smoothed template. `eyesim` implements these as `fixation_overlap`,
`sample_density`, and a hand-rolled nearest-cell lookup, respectively. What `integrate` cannot give
you is anything order-dependent; those statistics stay on `Scanpath` and are handled in Layer 4.

Grids have **nominal identity**, following `linop4s`'s `SpaceId`:

```scala
final case class GridId(name: String)
final class Grid[U <: Unit2D] private (
    val id: GridId, val frame: Frame[U], val nx: Int, val ny: Int
):
  def cellArea: Double
  def centres: IArray[Pt]
  def index(p: Pt): Option[Int]
```

Two grids of the same dimensions over different frames are **not** interchangeable. `eyesim` checks
grid compatibility in exactly one place (`Ops.eye_density`), compares vector *lengths* in a second,
checks `dim()` in a third, and does not check at all in `similarity.density` — so two maps built
with different default bounds are silently correlated cell-by-cell.

The invariant that `eyesim` most badly needs — *is this thing a probability mass or a signed
difference?* — becomes the type:

```scala
sealed trait Surface[U <: Unit2D]:
  def grid: Grid[U]
  def values: IArray[Double]
  def provenance: Provenance          // smoother, bandwidth, weighting, normalisation history

final class Mass     [U <: Unit2D] private (...) extends Surface[U]  // >= 0, sums to 1
final class Intensity[U <: Unit2D] private (...) extends Surface[U]  // >= 0, arbitrary scale
final class Signed   [U <: Unit2D] private (...) extends Surface[U]  // any real

/** A typed grid of non-Double values; `Region.rasterise` produces Field[U, Boolean]. */
final class Field[U <: Unit2D, A] private (val grid: Grid[U], val values: IArray[A])

object Mass:
  def of[U <: Unit2D](g: Grid[U], vs: IArray[Double]): Either[NotAMass, Mass[U]]
  def mean[U <: Unit2D](ms: NonEmptyVector[Mass[U]]): Either[GridMismatch, Mass[U]]
  def weightedMean[U <: Unit2D](ms: NonEmptyVector[(Double, Mass[U])]): Either[GridMismatch, Mass[U]]

extension [U <: Unit2D](i: Intensity[U])
  /** The ONLY route from an unnormalised estimate to a probability mass. */
  def normalised: Either[DegenerateSurface, Mass[U]]

extension [U <: Unit2D](m: Mass[U])
  def difference(that: Mass[U]): Either[GridMismatch, Signed[U]]
  def logRatio  (that: Mass[U]): Either[GridMismatch, Signed[U]]
  def entropy(base: LogBase = LogBase.E): Entropy          // defined on Mass ONLY

// Core defines its own module structure; `algebra` has no VectorSpace and Spire is rejected.
trait Module[V, K]:
  def zero: V
  def plus(a: V, b: V): V
  def scale(k: K, v: V): V

extension [U <: Unit2D](g: Grid[U])
  def signedModule: Module[Signed[U], Double]      // a method on the grid VALUE, not a given
```

Four consequences worth naming. `entropy` is an extension on `Mass` only, so calling it on a signed
difference map is a compile error — `eyesim` silently returns garbage. `Intensity.normalised` is the
single gate from an unnormalised estimate to a probability mass, replacing the roughly ten
sum-normalisation sites `eyesim` scatters across four files with four different epsilon guards.
`Mass.mean` over k maps exists — `eyesim` has no k-way average at all and its 2-way average is
spelled `+`. And the module structure is reached through a *grid value*, not summoned as a given, for
exactly the reason `linop4s` gives: `zero` depends on which grid you are in, and two grids in implicit
scope would be ambiguous.

Every surface also carries `Provenance` — the smoother, the bandwidth, the weighting, and whether it
has been normalised. `eyesim`'s `Ops.eye_density` drops `sigma` on every operation, which is why its
multiscale entropy method needs an `%||% NA_real_` fallback. A derived object should record how it
was derived; that is the whole content of trap 3 below.

Smoothing states its bandwidth convention in the type:

```scala
opaque type Sigma[U <: Unit2D] = Double     // ALWAYS a standard deviation, in frame units

trait Smoother[U <: Unit2D]:
  def bandwidth: Sigma[U]
  def smooth(m: PointMeasure[U], g: Grid[U]): Either[EstimateError, Intensity[U]]

object Smoother:
  def gaussian   [U <: Unit2D](s: Sigma[U]): Smoother[U]
  def anisotropic[U <: Unit2D](h: Mat2):     Smoother[U]
  def foveal     [U <: Unit2D](centre: Pt, s0: Sigma[U], slope: Double): Smoother[U]

object Bandwidth:
  def silverman2D[U <: Unit2D](m: PointMeasure[U]): Sigma[U]
  def scott      [U <: Unit2D](m: PointMeasure[U]): Sigma[U]
```

`Sigma[Deg]` is the psychologically meaningful choice — a 1° kernel is a statement about the fovea,
a 30 px kernel is a statement about nothing. Getting that for free is the clearest payoff of the
unit types.

Multi-scale is a real type that carries its scales, not a bare list with an attribute:

```scala
final class Pyramid[U <: Unit2D] private (val scales: NonEmptyVector[(Sigma[U], Mass[U])])
```

`eyesim`'s latent transforms silently take `obj[[1]]` from a multiscale object, discarding every
scale but the first.

Regions form a **Boolean algebra**, which is law-testable:

```scala
sealed trait Region[U <: Unit2D]:
  def contains(p: Pt): Boolean
  def area(g: Grid[U]): Double        // by rasterisation, at a STATED resolution
  def rasterise(g: Grid[U]): Field[U, Boolean]

object Region:
  def rect[U <: Unit2D](lo: Pt, hi: Pt): Region[U]
  def ellipse[U <: Unit2D](centre: Pt, rx: Double, ry: Double): Region[U]
  def polygon[U <: Unit2D](vs: NonEmptyVector[Pt]): Either[DegenerateRegion, Region[U]]
  def everything[U <: Unit2D]: Region[U]
  def empty[U <: Unit2D]: Region[U]
  extension [U <: Unit2D](a: Region[U])
    def ||(b: Region[U]): Region[U]
    def &&(b: Region[U]): Region[U]
    def unary_! : Region[U]
    def \(b: Region[U]): Region[U]
```

AOI statistics are then a thin, total layer:

```scala
final case class AoiSet[U <: Unit2D, K](frame: Frame[U], regions: ListMap[K, Region[U]])

extension [U <: Unit2D, K](s: AoiSet[U, K])
  def dwell(sp: Scanpath[U]): Map[K, Span]
  def firstEntry(sp: Scanpath[U]): Map[K, Option[Instant]]
  def runCount(sp: Scanpath[U]): Map[K, Int]
  def sequence(sp: Scanpath[U]): Vector[Option[K]]     // the ScanMatch string
  def transitions(sp: Scanpath[U]): Digraph[K]         // -> graph4s
```

Scalar summaries split along the duality, and the split is worth making explicit rather than filing
everything under "occupancy". **Order-free**, i.e. functionals of a `PointMeasure`: nearest-neighbour
index, BCEA, convex-hull area, stationary AOI entropy. **Order-dependent**, i.e. functions of a
`Scanpath` that a `PointMeasure` cannot support: scanpath length, saccade-amplitude sequence,
transition matrices, run counts, first-entry times, gaze-transition entropy, and the ambient/focal K
coefficient. Attempting the second set on a `PointMeasure` is a type error, because the order is
genuinely not there.

### Layer 4 — comparison

One honest signature. Comparison is *heterogeneous* — the interesting cases compare a map to a point
set — so the type has two sides:

```scala
trait Compare[-A, -B, +S]:
  def name: MeasureName
  def scale: MeasureScale
  def compare(a: A, b: B): Either[CompareError, S]

enum MeasureScale:                    // "a note on units" becomes a type
  case Correlation                    // [-1, 1]
  case FisherZ                        // unbounded
  case Probability                    // [0, 1]
  case Bounded(lo: Double, hi: Double)
  case DistanceLike                   // [0, inf), lower is closer
```

The lawful refinements, each with a Discipline rule set in `eyes4s-laws`:

```scala
trait Metric    [A] extends Compare[A, A, Distance]      // identity, symmetry, triangle
trait Semimetric[A] extends Compare[A, A, Distance]      // identity, symmetry
trait Divergence[A] extends Compare[A, A, Divergence.V]  // D(x,x) = 0, D >= 0, asymmetric OK
trait Kernel    [A] extends Compare[A, A, Similarity]    // symmetric, PSD Gram matrix
```

This is where the library gets to be honest in a way `eyesim` is not: cosine similarity is not a
metric; `1/(1 + W₁)` is a monotone transform of a metric and not one itself; entropic-regularised
Sinkhorn has `d(x, x) > 0` so self-similarity is *not* 1; MultiMatch's median-of-distances aggregate
breaks the triangle inequality. Each ships under the interface it actually satisfies, and the law
suite proves it.

Results are typed, which deletes `eyesim`'s 95-line `flatten_similarity_output` machinery and its
`expand_vector_output <- identical(method, "multimatch")` string test:

```scala
final case class MultiMatchScore(
    shape: Score, direction: Score, length: Score, position: Score, duration: Score)

final case class ScaleProfile[U <: Unit2D](byScale: NonEmptyMap[Sigma[U], Score]):
  def aggregate(a: Aggregation): Score
```

**Alignment is factored out.** MultiMatch, ScanMatch, DTW, Levenshtein and Fréchet are all "align
two sequences under a cost model, then summarise":

```scala
trait Alignment:
  def align[A, B](xs: IndexedSeq[A], ys: IndexedSeq[B])(cost: (A, B) => Double): AlignmentPath

final case class AlignmentPath(pairs: Vector[(Int, Int)], cost: Double)

object Alignment:
  val monotoneLattice: Alignment                   // MultiMatch's graph, as an O(nm) DP
  def needlemanWunsch(gap: Double, sub: SubstitutionMatrix): Alignment   // ScanMatch
  def dtw(band: Option[Int]): Alignment
  val frechet: Alignment
```

`eyesim` builds an igraph object and runs Dijkstra for MultiMatch. The lattice is a DAG with a
topological order given by `i + j`; the correct algorithm is a two-line dynamic program, exact and
far faster, with no graph dependency.

**Lifting is the compositional payoff.** `template_similarity` stops being a 1,800-line orchestrator
and becomes a combinator:

```scala
extension [U <: Unit2D](c: Compare[Mass[U], Mass[U], Score])
  def viaSmoothing(s: Smoother[U], g: Grid[U]): Compare[Scanpath[U], Scanpath[U], Score]
```

And the slot `eyesim` lacks entirely — map against point set — is now expressible, which brings the
whole saliency-benchmark literature into scope:

```scala
object Saliency:
  def nss         [U <: Unit2D]: Compare[Mass[U], PointMeasure[U], Score]
  def aucJudd     [U <: Unit2D]: Compare[Mass[U], PointMeasure[U], Score]
  def aucBorji    [U <: Unit2D](rng: Seed, nSplits: Int): Compare[Mass[U], PointMeasure[U], Score]
  def shuffledAuc [U <: Unit2D](others: Vector[PointMeasure[U]]): Compare[Mass[U], PointMeasure[U], Score]
  def infoGain    [U <: Unit2D](baseline: Mass[U]): Compare[Mass[U], PointMeasure[U], Score]
```

Distribution measures ship as `Compare[Mass[U], Mass[U], _]`: Pearson, Spearman, Fisher-z, cosine,
Tanimoto, total variation, KL, Jensen–Shannon, χ², Hellinger, exact W₁ and Sinkhorn-regularised OT
with λ exposed and its consequences documented.

### Layer 5 — design: trials, relations, pairings, reductions

The key insight, and the one that kills `eyesim`'s flagship bug: **the join key is a type, not a
string naming a column.**

```scala
final case class Trial[K, M, A](key: K, meta: M, value: A)
final case class Trials[K, M, A](rows: Vector[Trial[K, M, A]]):
  def mapV[B](f: A => B): Trials[K, M, B]
  def traverseV[E, B](f: A => Either[E, B])
      : (Trials[K, M, B], Vector[TrialTransformFailure[K, M, A, E]])

sealed trait Relation[L, R]
object Relation:
  final case class All[L, R]() extends Relation[L, R]
  final case class SameOn[L, R, J](
      left: Projection[L, J], right: Projection[R, J]
  ) extends Relation[L, R]
  final case class DifferentOn[L, R, J](
      left: Projection[L, J], right: Projection[R, J]
  ) extends Relation[L, R]
  final case class And[L, R](
      left: Relation[L, R], right: Relation[L, R]
  ) extends Relation[L, R]

sealed trait PairDesign[L, R]
object PairDesign:
  final case class BetweenDirected[L, R](
      relation: Relation[L, R], selection: Selection
  ) extends PairDesign[L, R]
  final case class WithinDirected[K](
      relation: Relation[K, K], self: SelfPolicy, selection: Selection
  ) extends PairDesign[K, K]
  final case class WithinUndirected[K](
      relation: Relation[K, K], self: SelfPolicy
  ) extends PairDesign[K, K]

/** Convenient constructors compile to Relation + PairDesign values. */
object Pairing:
  def matched[K]: PairDesign.BetweenDirected[K, K]
  def matchedOn[K, J](proj: Projection[K, J]): PairDesign.BetweenDirected[K, K]
  def mismatchedWithin[K, G](stratum: Projection[K, G]): PairDesign.WithinDirected[K]
  def sampled[L, R](
      relation: Relation[L, R], cap: Int, seed: Seed, sampleId: SampleId
  ): Either[PairingError, PairDesign.BetweenDirected[L, R]]

/** Unmatched and ambiguous keys are DATA, not a warning(). */
sealed trait Paired[KL, ML, KR, MR, A, B]
final case class DirectedPaired[KL, ML, KR, MR, A, B](
    pairs: Vector[(Trial[KL, ML, A], Trial[KR, MR, B])],
    eligiblePairCount: Long,
    unmatchedLeft: Vector[KL],
    unmatchedRight: Vector[KR],
    ambiguous: PairingAmbiguities[KL, ML, KR, MR, A, B],
    pairSpace: PairSpace.BetweenDirected | PairSpace.WithinDirected)
    extends Paired[KL, ML, KR, MR, A, B]
final case class UndirectedPaired[K, M, A](
    pairs: Vector[(Trial[K, M, A], Trial[K, M, A])],
    eligiblePairCount: Long,
    unmatchedLeft: Vector[K],
    unmatchedRight: Vector[K],
    ambiguous: PairingAmbiguities[K, M, K, M, A, A],
    pairSpace: PairSpace.WithinUndirected)
    extends Paired[K, M, K, M, A, A]

final case class PairScore[KL, KR, E, S](
    left: KL, right: KR, result: Either[E, S])

sealed trait PairwiseAnalysis[KL, KR, E, S]
final case class DirectedPairwiseAnalysis[KL, KR, E, S](
    rows: Vector[PairScore[KL, KR, E, S]],
    diagnostics: PairingReport[KL, KR],
    provenance: Provenance)
    extends PairwiseAnalysis[KL, KR, E, S]
final case class UndirectedPairwiseAnalysis[K, E, S](
    rows: Vector[PairScore[K, K, E, S]],
    diagnostics: PairingReport[K, K],
    provenance: Provenance)
    extends PairwiseAnalysis[K, K, E, S]

final case class Analysis[K, S](
    rows: Vector[(K, Either[ReductionError[K], S])],
    diagnostics: ReductionReport[K],
    provenance: Provenance)

def evaluatePairs[KL, ML, KR, MR, A, B, S](
    pairs: DirectedPaired[KL, ML, KR, MR, A, B],
    inputs: ContentHash,
    comparison: Compare[A, B, S]
): DirectedPairwiseAnalysis[KL, KR, CompareError, S]

def evaluatePairs[K, M, A, S](
    pairs: UndirectedPaired[K, M, A],
    inputs: ContentHash,
    comparison: SymmetricCompare[A, S]
): UndirectedPairwiseAnalysis[K, CompareError, S]

/** Contrast needs a difference on the score type; for MultiMatchScore it is per-field. */
trait Contrastable[S]:
  def diff(observed: S, baseline: S): S
def contrast[K, S: Contrastable](a: Analysis[K, S], b: Analysis[K, S]): Analysis[K, S]
```

`Relation` is structural, not a predicate hidden behind `accepts`. `SameOn` can therefore execute as
a hash join, a plan can persist its named projections, and diagnostics can name the clause that
excluded an edge. Pair-design inhabitants encode the semantic dependencies: self policy exists only
within one collection, per-focal selection exists only for directed pairs, and canonical-undirected
evaluation requires `SymmetricCompare`.

`PairwiseAnalysis` is primary because a baseline audit and a repeated-view analysis both need to
know the two observations behind a score. `Analysis[K, S]` is the reduced result. Directed reductions
name their side; canonical-undirected reductions choose `meanEdges` or the explicitly mirrored
`meanByEndpoint`. A reduction also chooses `RequireAll` or `SuccessfulOnly(minSuccessful)` and
returns eligible, selected, successful, and failed counts.

The directed and undirected subtypes enforce those choices. A plain function cannot evaluate an
`UndirectedPaired`; the caller supplies `SymmetricEvaluator`, or uses a `SymmetricCompare`. Every
evaluation also supplies the input `ContentHash`. The resulting provenance records that identity,
the evaluator and scale, the complete pair policy, seed and sample identifier, and realized counts.
Generic non-`Compare` evaluators additionally provide `EvaluationInfo`.

The baseline is not a code path. It is the same evaluator applied to a different pair design:

```scala
val subjectImage = Projection.named("subject-image")((k: Key) => (k.subject, k.image))
val subject      = Projection.named("subject")((k: Key) => k.subject)
val image        = Projection.named("image")((k: Key) => k.image)

val target = PairDesign.between[Key, Key]
  .sameOn(subjectImage, subjectImage)
  .all

val control = PairDesign.between[Key, Key]
  .sameOn(subject, subject)
  .differentOn(image, image)
  .bottomK(50, Seed(1), SampleId("image-control"))

val inputDigest = ContentHash.combineAll(
  templates.rows.map(_.value.provenance.digest) ++
  probes.rows.map(_.value.provenance.digest))

val reduced = control.map { controlDesign =>
  val observed =
    evaluatePairs(pair(templates, probes, target), inputDigest, cmp)
      .meanByRight(FailurePolicy.RequireAll)
  val baseline =
    evaluatePairs(pair(templates, probes, controlDesign), inputDigest, cmp)
      .meanByRight(FailurePolicy.RequireAll)
  (observed, baseline)
}
```

`x-contrast` supplies the typed `Contrastable` combination of the two reduced analyses; pair
evaluation and reduction do not contain a separate baseline branch.

Every eligible directed pair receives a stable priority from `(Seed, SampleId, focal KeyDigest,
candidate KeyDigest)`. The cap and eligibility relation do not enter the priority, so bottom-60
contains bottom-50 and row order is irrelevant. `KeyDigest` derives through `Mirror`, obeys
`Eq(a, b) => digest(a) == digest(b)`, and has JVM/Scala.js golden vectors.

Repeated viewing uses the same pieces, but makes its scientific scope visible:

```scala
val repeated = PairDesign.within[Key]
  .sameOn(Projection.named("subject-image")(k => (k.subject, k.image)))
  .differentOn(Projection.named("phase")(_.phase))
  .excludingSelf
  .canonicalUndirected

val viewingInputs =
  ContentHash.combineAll(viewings.rows.map(_.value.provenance.digest))
val perViewing =
  evaluatePairs(pair(viewings, repeated), viewingInputs, cmp)
    .meanByEndpoint(FailurePolicy.RequireAll)
```

There is no participant default. Removing the named subject clause asks the distinct
cross-participant-consistency question. This is the seam `eyesim` lacks: its repetitive-similarity
vignette claims same-image, cross-phase scores while the implementation groups only on phase, mixes
participants and images, and places the intended reinstatement pair in its `othersim` baseline.

Domain adaptation (`eyesim`'s latent transforms) gets a real fit/apply split, with the change of
representation in the type — which makes "EMD after PCA" *unrepresentable* rather than a runtime
`match.arg` failure:

```scala
trait Adapter[A, B]:
  type Model
  def fit(pairs: Vector[(A, A)]): Either[FitError, Model]
  def toRef(m: Model, a: A): Either[ApplyError, B]
  def toSrc(m: Model, a: A): Either[ApplyError, B]

object Adapter:
  def pca  [U <: Unit2D](k: Int):                Adapter[Mass[U], Latent]
  def coral[U <: Unit2D](k: Int, shrink: Double): Adapter[Mass[U], Latent]
  def cca  [U <: Unit2D](k: Int, shrink: Double): Adapter[Mass[U], Latent]
  def contract[U <: Unit2D](shrink: Double):      Adapter[Mass[U], Mass[U]]   // stays a map
  def affine  [U <: Unit2D](shrink: Double):      Adapter[Mass[U], Mass[U]]
  extension [A, B, K, G](a: Adapter[A, B]) def stratifiedBy(f: K => G): Adapter[A, B]
```

Strata are keyed by a real type, not by the sentinel string `"__all__"` that `eyesim` then renames
to `"all"` — a collision waiting for the first study with a condition called `all`.

Surface decomposition reuses alignment and pair evaluation without pretending to be a `Compare`.
OLS returns a `Signed` fit, intercept-free NNLS an `Intensity`, and simplex-constrained fitting a
`Mass`; every residual is `Signed`. Only the simplex-constrained coefficients are mixture weights.
Diagnostics are descriptive — rank, conditioning, convergence, residual norms, and R² — because
cell-wise standard errors would falsely treat spatially autocorrelated cells as independent.
Partial association is a different result type, not a coefficient.

The convenient surface keeps distinct scientific verbs: matched similarity, repetition similarity,
surface decomposition, and temporal reinstatement. They are thin functions over the algebra above,
not a `Workflow` hierarchy or a single `TemplateAnalysis` engine.

---

## Modules and dependencies

### There is no dataframe in this design

`eyesim` uses a tibble for four jobs. Only one of them is table-shaped:

| Job | `eyesim` | What it actually is |
|---|---|---|
| the fixations of one trial | tibble | `IArray[Fixation]` — a homogeneous sequence of a known record |
| trials with a list-column | nested tibble | `Vector[Trial[K, M, A]]` — a keyed collection |
| results with appended columns | tibble | `PairwiseAnalysis`, reduced `Analysis`, plus an export at the edge |
| study metadata (subject, block, accuracy, RT) | tibble | **genuinely tabular** — schema unknown at library-compile time |

Modelling the first three as a dataframe is what produces `x[, 1:2]` silently meaning
`(index, x)`, nine parameter names for "the list-column", and `match_on` as a `character`. For the
library's own representations the dataframe is the disease, not the cure.

The fourth is real, and the Scala answer is not a dataframe but the user's own product type carried
as a parameter:

```scala
final case class Key(
    subject: SubjectId, image: ImageId, phase: Phase, repetition: RepetitionId)
final case class Meta(block: Int, correct: Boolean, rt: Span)

val trials: Trials[Key, Meta, Scanpath[Unit2D.Deg]] = ...
trials.filter(_.key.phase == Phase.Encoding)       // pairing dimensions live in the key
```

eyes4s never inspects `Meta`; it carries it. Reading it from a study CSV is a `Mirror`-derived
decoder in `eyes4s-io` (no dependency). Getting results back out — to R, in practice — is a CSV or
Arrow writer, not a query engine. `frame4s` therefore becomes an optional *export adapter*, which
also dissolves the LTS-vs-3.7.4 tension: core never needs named tuples.

### What actually needs an external library

Working through every algorithm in the design, the answer is: almost nothing.

**Needs only `cats-core` + core's own algebra.** Kernel density estimation — a Gaussian on a grid is
a *separable convolution*, two 1-D passes over `Array[Double]`, so the library's heaviest computation
needs no linear algebra at all. Every distribution measure (Pearson, Spearman, cosine, Tanimoto,
total variation, KL, Jensen–Shannon, χ², Hellinger). Every alignment (Needleman–Wunsch, DTW, Fréchet,
and MultiMatch's lattice as an O(nm) DP — which deletes `eyesim`'s `igraph` dependency, since the
lattice is a DAG ordered by `i + j` and Dijkstra was never required). Event detection and filtering,
with Savitzky–Golay as precomputed coefficient sets. CRQA. AUC-family metrics. Convex hull, BCEA,
point-in-polygon. Sinkhorn OT, which is iterated matrix–vector scaling in about fifty lines, and on a
grid reuses the separable Gaussian kernel already present.

**Needs a real external library — one thing.** `gale`, for SVD, matrix square root and inverse square
root, used *only* by the latent adapters (PCA, CORAL, CCA). It goes in an optional module;
`eyes4s-design` stays core-only for pairings and baselines.

**Needed at the edges.** `fs2` + `cats-effect` for streaming parsers and online detection; a JSON
codec (`circe`, already in the house stack) for BIDS sidecars; `intaglio` for viz; munit + ScalaCheck
+ discipline as *main* dependencies of `eyes4s-laws`, per house convention.

**Two things have no Scala implementation anywhere.** Exact W₁ is a transportation LP (network
simplex, ~500 lines) — note that `eyesim`'s three EMD backends are not numerically equivalent in any
case (exact-unnormalised vs entropic-normalised vs exact-normalised), so shipping Sinkhorn and
sliced-W₁ first and treating exact EMD as a later optional module costs nothing real. And binary EDF
parsing requires SR Research's proprietary `edfapi`; out of scope, require `edf2asc`.

### Module table

Following the `linop4s` / `graph4s` lineage: own repo, own `build.sbt`, top-level module
directories, `tlCrossRootProject`, and the `checkModuleBoundaries` task copied verbatim so that no
pure module can transitively acquire `cats-effect` or `fs2`.

| Module | Depends on | Platforms | Contents |
|---|---|---|---|
| `eyes4s-kernel` | cats-core | JVM/JS/Native | **no ocular vocabulary**: units, frames, warps, clocks, intervals, trajectories, point measures, grids, surfaces, regions, `Machine`, and the library's own `Module` |
| `eyes4s-core` | kernel | JVM/JS/Native | the eye-specific layer: `Gaze`, `Sample`, `Recording`, `Eye`, `Fixation`, `Saccade`, `Blink`, `Pursuit`, `Scanpath`, `Viewing` |
| `eyes4s-detect` | core | JVM/JS/Native | `Detector` instances, filters, I-VT / I-DT / Engbert–Kliegl / NH / I2MC |
| `eyes4s-surface` | core | JVM/JS/Native | smoothers, bandwidth selection, pyramids, entropy |
| `eyes4s-aoi` | core | JVM/JS/Native | AOI sets, dwell/entry/run statistics, transition matrices |
| `eyes4s-compare` | core, surface, aoi | JVM/JS/Native | `Compare` hierarchy, alignment kernel, MultiMatch, ScanMatch, CRQA, distribution measures, Sinkhorn/sliced OT, saliency metrics |
| `eyes4s-design` | core, compare | JVM/JS/Native | trials, pairings, baselines, contrasts, deterministic RNG |
| `eyes4s-laws` | all pure modules + munit, scalacheck, discipline-munit (**main** deps) | JVM/JS/Native | rule sets: warp category, region Boolean algebra, surface module, metric axioms, kernel PSD, machine composition |
| `eyes4s-gale` | core, design, gale-core | JVM/JS | SVD-backed adapters (PCA, CORAL, CCA); exact EMD later |
| `eyes4s-graph4s` | aoi, graph4s-core | JVM/JS/Native | transition matrices as `Digraph[K]`, for graph algorithms |
| `eyes4s-fs2` | core, detect, fs2, cats-effect | JVM/JS | `Machine.toPipe`, streaming windows, online detection |
| `eyes4s-io` | fs2 module, circe | JVM/JS | EyeLink ASC, Tobii TSV, SMI, BIDS eye-tracking, CSV in/out, `Mirror`-derived metadata decoders |
| `eyes4s-viz` | core, surface, intaglio | JVM/JS | plot specifications: scanpath, heat map, AOI overlay, pyramid, difference map with a zero-anchored diverging scale |
| `eyes4s-frame4s` | frame4s-core | JVM/JS | optional projection of `Analysis` into a typed `Frame` |

Two boundaries are enforced mechanically, by a `checkModuleBoundaries` task that inspects the
resolved dependency graph rather than by documentation:

1. **No pure module may acquire `cats-effect` or `co.fs2`** — copied verbatim from `linop4s`.
2. **`eyes4s-kernel` may not depend on `eyes4s-core`.** This is what keeps the trajectory-and-measure
   layer free of ocular vocabulary, and it is the entire cost of leaving the door open to mouse
   tracking, animal tracking, and VR navigation later. See "Beyond gaze: the trajectory kernel".

Note what is *not* in the kernel: no `algebra` (the vector-space layer is defined here, as `linop4s`
does, because per-grid `zero` is not a global constant), no `gale`, no `graph4s`, no `frame4s`, no
effects, and no eyes.

Build settings: Scala 3.3.8 LTS pinned as `val Scala3`, sbt 1.11.7, sbt-typelevel 0.8.7,
`tlBaseVersion := "0.1"`, `tlJdkRelease := Some(11)`, Apache-2.0, org `io.github.canardlapin`,
`.scalafmt.conf` copied from `linop4s` (scalafmt 3.11.4, `maxColumn = 96`, no optional-brace
rewrite). `eyes4s-frame4s` carries a per-project `scalaVersion := 3.7.4` override for named tuples
and is excluded from the Native axis; nothing else in the build is affected.

---

## Worked examples

Examples elide constructors that the sections above name but do not spell out (`Grid.of`,
`Sigma.deg`, `Compare.fisherZ`, `Prior.centreBias`) and the unit literals `mm` / `ms` / `deg` /
`degPerSec`. They do not elide error handling: where a signature returns `Either`, the example
handles it.

### 1. The `eyesim` flagship pipeline, correct by construction

```scala
import eyes4s.*, eyes4s.surface.*, eyes4s.compare.*, eyes4s.design.*

final case class Key(subject: SubjectId, image: ImageId, phase: Phase)
final case class Meta(correct: Boolean)

val screen = Frame.screen("bench", 1280, 1024)                     // Frame[Px], y-down
val fovea  = Frame.angular("fovea", Extent(34.0, 27.0))            // Frame[Deg]
val toDeg  = Warp.tangent(screen, fovea, Viewing(mm(600), PhysicalScreen(mm(530), mm(300))))

val grid     = Grid.of(fovea, 128, 128)
val smoother = Smoother.gaussian(Sigma.deg(1.0))                   // an SD, in degrees

def mapOf(sp: Scanpath[Unit2D.Deg]): Either[EstimateError, Mass[Unit2D.Deg]] =
  smoother.smooth(PointMeasure.fromScanpath(sp, Weight.Duration), grid)
          .flatMap(_.normalised)                                   // Intensity => Mass

val trials: Trials[Key, Meta, Scanpath[Unit2D.Deg]] = load(...)
val (templates, tErr) = trials.filter(_.key.phase == Phase.Encoding).traverseV(mapOf)
val (probes,    pErr) = trials.filter(_.key.phase == Phase.Retrieval).traverseV(mapOf)

val cmp = Compare.fisherZ[Mass[Unit2D.Deg]]

val subjectImage = Projection.named("subject-image")((k: Key) => (k.subject, k.image))
val subject      = Projection.named("subject")((k: Key) => k.subject)
val image        = Projection.named("image")((k: Key) => k.image)

val target = PairDesign.between[Key, Key]
  .sameOn(subjectImage, subjectImage)
  .all
val control = PairDesign.between[Key, Key]
  .sameOn(subject, subject)
  .differentOn(image, image)
  .bottomK(50, Seed(1), SampleId("image-control"))

val inputDigest = ContentHash.combineAll(
  templates.rows.map(_.value.provenance.digest) ++
  probes.rows.map(_.value.provenance.digest))

val reduced = control.map { controlDesign =>
  val observed =
    evaluatePairs(pair(templates, probes, target), inputDigest, cmp)
      .meanByRight(FailurePolicy.RequireAll)
  val baseline =
    evaluatePairs(pair(templates, probes, controlDesign), inputDigest, cmp)
      .meanByRight(FailurePolicy.RequireAll)
  (observed, baseline)
}
```

Read what is no longer possible. The target relation names subject and image, while the control
relation names same-subject and different-image. Neither participant scope nor baseline semantics can
be inherited accidentally from row order. Trials that failed density estimation are in `tErr`/`pErr`,
not silently dropped with a warning that changes the row count. `Sigma.deg(1.0)` cannot be handed to
a smoother over a pixel grid. And both map sets share `grid` by construction, so cell-by-cell
correlation of incommensurable maps is not expressible.

### 2. Raw samples to AOI dwell, in degrees

```scala
val pipeline: Machine[Sample[Unit2D.Deg], Event[Unit2D.Deg]] =
  Filter.deblink[Unit2D.Deg](maxGap = ms(75), pad = ms(20))
    .andThen(Filter.savitzkyGolay(window = 7, order = 2))
    .andThen(Detector.ivt(threshold = degPerSec(30), minDuration = ms(60)))
    .andThen(Merge.adjacentFixations(maxGap = ms(75), maxAngle = deg(0.5)))

val aois = AoiSet(fovea, ListMap(
  Face -> Region.ellipse[Unit2D.Deg](Pt(-3, 4), 2.5, 3.0),
  Text -> Region.rect[Unit2D.Deg](Pt(-8, -6), Pt(8, -2))))

val result: Either[Eyes4sError, Map[Aoi, Span]] =
  for
    inDeg <- recording.warp(toDeg)                  // Recording[Px] => Recording[Deg]
    events = pipeline.runAll(inDeg.samples)
    sp    <- Scanpath.fromEvents(fovea, inDeg.clock, events)
    dwell <- aois.dwell(sp)                         // frame ids checked here
  yield dwell
```

`pipeline` consumes `Sample[Deg]`, not `Sample[?]`, so `recording.warp(toDeg)` is not a stylistic
choice — the pipeline will not accept the pixel recording at all, and producing `toDeg` requires a
`Viewing`. That is the unit-safety claim made good at the level of the *data*, not merely the
threshold literal.

### 3. Saliency model evaluation — the slot `eyesim` does not have

```scala
val fixations: PointMeasure[Unit2D.Norm] = PointMeasure.fromScanpath(sp, Weight.Uniform)
val model:     Mass[Unit2D.Norm]         = loadSaliencyMap(image)
val centre:    Mass[Unit2D.Norm]         = Prior.centreBias(grid)

val scores: Either[CompareError, (Score, Score, Score)] =
  for
    nss  <- Saliency.nss.compare(model, fixations)
    sauc <- Saliency.shuffledAuc(otherImagesFixations).compare(model, fixations)
    ig   <- Saliency.infoGain(centre).compare(model, fixations)
  yield (nss, sauc, ig)
```

### 4. The same detector, online

```scala
tracker.samples[IO]                       // fs2.Stream[IO, Sample[Px]]
  .through(Warp.pipe(toDeg))
  .through(pipeline.toPipe)               // the identical Machine value from example 2
  .collect { case f: Fixation[Unit2D.Deg] => f }
  .evalMap(f => if aois.regions(Face).contains(f.centre) then stimulus.reveal else IO.unit)
  .compile.drain
```

One detector definition, law-tested once, used offline and in a gaze-contingent loop. The caveat
from Layer 2 applies: on an unbounded stream `flush` never runs, so the fixation in progress when
the trial ends is emitted only once the stream terminates.

One detector definition, law-tested once, used offline and in a gaze-contingent loop.

---

## What this opens up

`eyesim` answers one question. The point of the design above is not that it answers that question
more safely — it is that several subfields that `eyesim` cannot structurally reach become short
modules rather than new libraries. Five properties do the work.

| Unlock | Consequence |
|---|---|
| **The atom is a sample, not a fixation** | velocity, pupil, microsaccades, pursuit, drift, and every data-quality measure become addressable. `eyesim`'s atom is a fixation, and that one choice excludes an entire tier of the field. |
| **Degrees of visual angle are reachable and enforced** | a large class of measures is meaningful *only* in visual angle; in a pixel-only library they are quietly wrong |
| **Comparison is heterogeneous** (`Compare[-A, -B, +S]`) | opens three slots `eyesim` cannot express: map-vs-points, model-vs-data, and second-order (RDM-vs-RDM) |
| **Design is data** (`Pairing`) | "match vs mismatch" becomes one instance of general resampling inference, including per-grid-cell permutation |
| **Detectors run online and cross-compile to JS** | experiments, not only analysis — in the same law-tested code |

### Near-free: days, not months

Each of these is a thin module over primitives the core already has.

| Analysis | Primitive it rides on | Subfield |
|---|---|---|
| **Reading measures** — first-fixation duration, gaze duration, go-past time, regression-path duration, first/second pass, regressions in and out | `AoiSet` + `Interval` + `Overlap`; pure interval algebra over an AOI sequence | reading research, arguably the largest subfield of eye tracking, and entirely outside `eyesim` |
| **Saliency benchmarking** — AUC-Judd, AUC-Borji, shuffled AUC, NSS, CC, SIM, KLD, information gain | `Compare[Mass[U], PointMeasure[U], Score]` | computational attention; a whole community built on this one primitive |
| **Microsaccades and the main sequence** — rate signatures, direction bias under covert attention, amplitude-vs-peak-velocity | `Detector.engbertKliegl` + `Velocity[Deg]` | oculomotor control, covert attention |
| **Data-quality battery** — RMS-S2S precision, accuracy against known targets, data loss, tracking ratio | raw `Sample[Deg]` | increasingly required for publication; impossible without raw samples |
| **Statistical mapping over the stimulus plane** — pixel-wise contrasts with cluster-based permutation inference | `Signed` maps + per-grid `Module` + a permutation `Pairing` | what `iMap` does in MATLAB, native here |
| **Ambient/focal dynamics** — the K coefficient | saccade amplitude and fixation duration z-scored in commensurable units | scene viewing, expertise, ageing |

### Larger expansions the design invites

- **Pupillometry.** `Gaze.Tracked` already carries pupil. Baseline correction, luminance regression,
  and deconvolution against a pupil response function — which is an HRF-shaped convolution problem.
  `fmrihrf` supplies the basis machinery and `linop4s` the regularised least squares. This synergy
  is not a coincidence; it is the same mathematics.
- **RSA for gaze.** `Compare` + `Trials` + `Pairing` already give a condition × condition similarity
  matrix; comparing it against a model RDM is a second-order `Compare` and a short module. The
  cross-modal question — does the gaze RDM track the neural RDM? — is then directly available, and
  is adjacent to `rMVPA`.
- **Generative scanpath models.** Once a predicted map can be scored against observed fixations,
  models (SceneWalk, ideal-observer, CLE) can be fit and compared through
  `Compare[Model, Scanpath[U], LogLik]`.
- **HMM and state-space scanpath analysis.** EMHMM-style clustering of observers by AOI dynamics,
  and I-HMM event detection. The `Machine` abstraction is already the right substrate.
- **Dynamic stimuli and gaze synchrony.** `Moving[A, B]` gives dynamic AOIs on video, inter-subject
  correlation of attention, and synchrony time courses — film cognition, social attention, autism
  research.
- **Dual eye tracking.** Cross-recurrence between two observers' scanpaths: joint attention,
  conversational coupling.
- **Gaze-contingent paradigms.** Moving window, boundary paradigm, foveated rendering — analysis and
  experiment in one cross-compiled library.

### Beyond gaze: the trajectory kernel

The core abstraction is not ocular. *A timed trajectory through a typed geometry, and the measure it
induces on that geometry* describes mouse and cursor tracking (maximum deviation, area under the
curve, x-flips are trajectory functionals), stylus and drawing data, animal tracking (open-field
occupancy maps, arena dwell, exploration entropy are `PointMeasure` + `Region` verbatim), VR
navigation paths, and — given a 3D extension — reaching kinematics.

`Warp`, `Region`, `PointMeasure`, `Grid`, `Surface`, `Interval`, `Machine` and `Compare` contain no
ocular vocabulary. Only `Gaze`, `Sample`, `Recording`, `Fixation`, `Saccade`, `Blink`, `Pursuit`,
`Scanpath` and `Viewing` do.

**The recommendation is not to build that generalisation now.** It is to make the boundary a
*module* rather than a convention, so the door stays open at zero present cost: `eyes4s-kernel`
holds the trajectory-and-measure layer and **cannot depend on `eyes4s-core`**, enforced by the same
dependency-graph task that keeps effects out of pure modules. If a `trace4s` is ever worth
extracting, it is already extracted.

One further bridge worth noting: gaze–fMRI co-registration — *what was being fixated during this
TR?* — is an interval-algebra and clock-synchronisation problem, and `Sync` and `Interval` already
exist. With `neuroimsc` and `fmridesignsc` in the same ecosystem that is a short module, not a
project.

### Suggested order

For maximum reach per unit of effort: **reading measures → saliency benchmarking → data quality and
microsaccades → statistical mapping → pupillometry**. The first three are near-free given the core,
each opens a subfield `eyesim` cannot reach, and none of them requires a new abstraction.

---

## Design traps to avoid

Each of these is drawn from the `eyesim` read, and each is a decision this design makes
deliberately.

1. **Never let a bandwidth parameter mean different things in different backends.** `Sigma[U]` is a
   standard deviation in frame units, always. A backend that wants a different convention converts
   at its own boundary.
2. **Never use `null`/`None` as an undifferentiated error channel.** `eye_density` returns `NULL` for
   five distinct failures and every consumer re-implements the check differently — one of which
   (`filter(!(is.null(a) | is.null(b)))` on a list-column) never fires. Return
   `Either[EstimateError, _]` with an error ADT.
3. **Never put the source data inside the derived object.** Every `eye_density` carries a full copy
   of its fixations while discarding the bounds, normalisation flag, and weighting scheme actually
   needed to interpret it. Derived objects carry *provenance*, not *inputs*.
4. **Never emulate a sum type with a named vector.** One measure returning a scalar and another
   returning six named doubles is what produces `flatten_similarity_output`, `format_similarity_result`,
   and `expand_vector_output <- identical(method, "multimatch")`.
5. **Never pad an (n−1)-sequence to length n.** Saccades are one fewer than fixations. Say so.
6. **Never let a `method` string be anything other than a dispatch key.** `eyesim` maintains five
   separately-drifting `match.arg` lists of the same names — `"emd"` is already missing from one —
   while real dispatch happens on the runtime class of a list-column element.
7. **Never discard the geometry after construction.** If a constructor computes the coordinate
   frame, the frame is a member of the result.
8. **Never let an operator mean something other than its name.** `+` is not a mean; `/` is not a
   log-ratio. Name them `mean` and `logRatio`.
9. **Never index columns positionally.** `x[, 1:2]` is `(index, x)` on one construction path and
   `(x, y)` on another; `estimate_scale` fits on the wrong pair as a result.
10. **Never warn-and-drop.** Unmatched keys, ambiguous keys, and failed rows are returned as data.
    A function must not silently change its caller's row count.
11. **Never implement a baseline more than once.** One `Pairing`, one sampler, one realised count.
12. **Never normalise in ten places.** `eyesim` sum-normalises at roughly ten sites with four
    different epsilon guards. Normalisation happens once, in the `Mass` constructor, and the type
    records that it happened.
13. **Never let a declared abstraction go uninhabited.** `density_matrix` is exported with zero
    methods. A trait with no instance and no law suite does not ship.
14. **Never mutate global randomness.** `set.seed(seed)` inside a fold-construction helper changes
    the caller's RNG. Seeds are threaded; fold assignment is a deterministic function of the key.
15. **Never discover a backend by probing at the bottom of a loop.** `requireNamespace` guards
    embedded in compute paths silently change both the estimator and its bandwidth semantics.
    Backends are typeclass instances in named modules, selected explicitly.
16. **Never route an order-dependent statistic through an order-free representation.** The
    occupancy view is a genuine simplification, and the temptation is to make it the universal
    one. It cannot carry MultiMatch, ScanMatch, CRQA, transitions, or scanpath length. Keep
    `occupancy: Scanpath => PointMeasure` explicit and lossy, and let the type refuse.
17. **Never let a unit live only in a parameter.** A threshold typed `Velocity[Deg]` proves nothing
    if the samples it consumes are untyped `Pt`s — the compiler checks the literal, not the data.
    The unit belongs on `Sample`, `Event`, and `Scanpath`, or the guarantee is theatre. This one
    was caught in review of an earlier draft of this document, which is the best evidence that the
    trap is easy to fall into.

---

## Implementation status and roadmap

Nothing is built. The proposed order, each milestone with an acceptance criterion.

| # | Milestone | Acceptance criterion |
|---|---|---|
| 0 | Repo skeleton: build, scalafmt, `AGENTS.md`, CI, module boundaries | both boundary rules enforced (`checkModuleBoundaries` passes, and `eyes4s-kernel` provably does not depend on `eyes4s-core`); `testAll` green on JVM/JS/Native |
| 1 | **Kernel geometry + core events**: units, frames, warps, clocks, intervals; then samples, events, scanpaths | `eyes4s-laws` proves warp composition associative and identity-respecting on the matching-frame subcategory; round-trip `px -> deg -> px` within stated tolerance; `Scanpath` smart constructor rejects non-monotone onsets; kernel compiles with `eyes4s-core` absent from the classpath |
| 2 | **Occupancy** (kernel): point measures, grids, surfaces, regions | Region Boolean-algebra laws (Discipline); `Signed` module laws per grid; `Mass` constructor proves non-negativity and unit sum; `integrate` against an indicator equals `massIn` |
| 3 | **Detect**: I-VT, I-DT, Engbert–Kliegl, filters, `Machine` composition | Category laws for `Machine` stated as observational equality on output sequences; `runAll` and `toPipe` produce identical output on the same **finite** input (property test); agreement with published reference implementations on a shared fixture set |
| 4 | **Surface + compare**: smoothers, bandwidth, pyramids; metric hierarchy, alignment kernel, MultiMatch, distribution measures | Metric axioms law-tested per instance; MultiMatch matches the `multimatch-gaze` Python reference on the `eyesim` parity fixtures at `grouping = FALSE`; `monotoneLattice` DP reproduces the Dijkstra path exactly |
| 5 | **Design**: trials, relations, pair designs, edge results, reductions, decomposition | Relation truth tables and sampling mutants are green; cap-monotone keyed samples and `KeyDigest` golden vectors agree on JVM/JS; parity fixtures cover matched similarity and the deliberate repetitive-similarity divergence |
| 6 | **IO**: EyeLink ASC, BIDS eye-tracking, CSV | Round-trip a real EDF-derived ASC; parse a BIDS eye-tracking dataset end to end |
| 7 | **AOI + reading measures** | first-fixation duration, gaze duration, go-past time, regression-path duration and regression counts reproduced against a published reading corpus |
| 8 | **Saliency metrics, viz, frame4s interop** | MIT/Tübingen benchmark metric values reproduced on a published fixture |
| 9 | **Statistical mapping** | pixel-wise contrast with cluster-based permutation inference; false-positive rate at the nominal level on null data |

Milestones 1–2 are the ones worth getting right; everything after is comparatively mechanical.
Milestones 7–9 are the first ones that go *beyond* what `eyesim` can express, and are the point of
the whole exercise — see "What this opens up".

---

## Relationship to sibling libraries

- **`gale`** — dense/sparse linear algebra. Supplies SVD for PCA/CORAL/CCA and the dense kernels
  behind OT. `eyes4s-core` does not depend on it; `eyes4s-gale` does.
- **`linop4s`** — matrix-free operators and Krylov solvers. Not a core dependency, but the natural
  home for anything regularised-inverse-shaped: deconvolving pupil responses, smoothing operators
  with adjoints, drift correction as a least-squares problem. eyes4s borrows two of its design
  decisions wholesale — spaces (here, grids and frames) as values with nominal identity, and
  capabilities as types rather than docstrings.
- **`graph4s`** — AOI transition digraphs, scanpath networks, connected components on thresholded
  maps. *Optional*: a transition matrix is a k×k array and `eyes4s-aoi` computes it unaided; the
  `Digraph[K]` projection lives in `eyes4s-graph4s` for users who want graph algorithms on it.
- **`frame4s`** — typed local dataframe. *Optional export adapter only.* Study metadata is the user's
  own product type carried as a parameter, so core needs no dataframe, no named tuples, and no
  departure from the LTS or the Native axis. See "There is no dataframe in this design".
- **`fmrihrf`** — HRF bases and convolution. The pupil response function is the same object under a
  different name; pupillometric deconvolution should borrow its basis machinery rather than
  re-derive it.
- **`neuroimsc` / `fmridesignsc`** — gaze–fMRI co-registration is an interval-algebra and
  clock-synchronisation problem that `Sync` and `Interval` already solve; and the statistical-mapping
  milestone is the fMRI cluster-inference method applied to the stimulus plane.
- **`resample4s`** (spec only) — cross-validation, bootstrap, leakage safety. `eyes4s-design` ships
  the minimum needed for `template_similarity_cv` parity and should hand the general machinery to
  `resample4s` when it exists. The phase-restricted `Fit`/`Eval` wrappers proposed there are exactly
  what `eyesim`'s `fit_source_filter` / `eval_source_filter` are groping toward.
- **`intaglio`** — grammar of graphics. `eyes4s-viz` emits specifications; intaglio renders them on
  JVM and in the browser.
- **`repro4s`** (spec only) — provenance and content-addressed artifacts. `Analysis.provenance` is
  designed to be a `repro4s` manifest fragment when that exists.

---

## Design questions

**Resolved.**

- *Units static, frames nominal-runtime.* Full dependent typing of frames would be unusable for real
  data — you cannot know at compile time which of 200 images a trial belongs to. Px-vs-deg is
  knowable and is the dominant error, so it is static. This follows `linop4s`'s `Space` precedent.
- *Core defines its own `Module`; not Spire, and not `algebra` either.* Spire has not been migrated
  to Scala Native 0.5 and its array instances have wrong `zero` semantics for a per-space carrier.
  `algebra` has no vector-space abstraction at all, so there is nothing to inherit — `linop4s`
  defines `VectorSpace`/`InnerProductSpace` itself for the same reason, and core's only dependency
  is `cats-core`.
- *No tagless final in core.* A user doing pure synchronous analysis never touches an effect type.
  Effects live in `eyes4s-fs2` and are mechanically excluded from everything else.
- *Non-convergence and failed estimation are values.* `Either` with error ADTs; exceptions reserved
  for defects (a grid mismatch is an `IllegalArgumentException` at construction, never inside a
  loop).
- *Offsets are stored.* `Interval(onset, offset)`, not `(onset, duration)`.

**Resolved 2026-07-24.** All questions previously open here have been decided and are recorded as
closed beads in the `mote` store at `.mote/`, each carrying a `decision`-kind note with its full
rationale. Source comments cite them in the house style as `bead q-<name>`. The full resolution table
is in [`PRD.md`](PRD.md) §Resolved Decisions; the outcomes that changed the design above are:

- **Time (`q-interval-clock`).** `Interval` is absolute and carries a `ClockId`; `Window` is relative
  — two `Span`s from a named anchor — and carries no clock. Splitting the type was chosen over
  tagging one, because an analysis window was never an absolute interval and conflating the two is
  its own bug class.
- **Frame checking (`q-scope`).** No `Scope` capability. Kernel binary operations stay `Either`-only;
  ergonomics come from `Session`, a checked container that validates frame membership on insertion
  and is therefore total on the way out. A capability value is forgeable; a container's contents were
  validated on entry.
- **Binocular (`q-binocular`).** `Recording` stays monocular; `BinocularRecording[U]` is a separate
  type with `left` / `right` / `cyclopean` projections and a vergence signal, landing at v0.3 so that
  ASC ingest cannot silently drop an eye.
- **Regions (`q-region-exact`).** Exact ADT; `contains` exact and resolution-independent, `area`
  parameterised by a `Grid`.
- **Bandwidth (`q-sigma-units`).** `Sigma[U]` stays in frame units, with `Sigma.deg` the documented
  default path.
- **Provenance (`q-provenance-cache`).** `Provenance` carries a `ContentHash` of its inputs. A
  parameter record alone cannot distinguish two datasets analysed identically, so provenance was
  never a valid cache key without it.
- **Plans (`q-plan-coverage`).** A fixed core vocabulary plus a typed extension registry —
  `NodeDef[P]` with a codec and an interpreter — where only lookup is by identifier.
- **CRQA (`q-crqa`).** Implemented properly, in v1.1, not v1.0.
- **Laws packaging (`q-laws-publication`).** One `eyes4s-laws` module; revisit post-1.0 on real
  demand only.
- **Application target (`q-app-target`).** A local JVM process serving a browser UI, decided by data
  governance rather than technology: gaze files are large and are human-subjects data. **Consequence:
  Scala Native remains unnecessary post-1.0, and the Scala.js target is confirmed load-bearing.**

Two build questions remain genuinely open and are tracked in the PRD rather than here: whether
`eyes4s-frame4s` carries a per-project `scalaVersion := 3.7.4` override for named tuples (proposal:
yes, as the sole non-uniform module), and whether an R-parity harness is worth its maintenance cost
(resolved as advisory, never a gate — see `PARITY.md` when it exists).
