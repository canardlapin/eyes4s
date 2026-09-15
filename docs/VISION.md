# Vision and mission

Direction established with the owner on 2026-09-08. This document states the product direction;
[the development plan](DEVELOPMENT_PLAN.md) defines the next milestones. It describes the intended
library, not a claim that every capability is implemented. The rationale is recorded in closed
Mote decision `bd-01M214TX04A7GNRHP3ECBXQETE`.

## Vision

**eyes4s will be a coherent foundation for eye-movement analysis: a library researchers enjoy
using, method developers can extend, and applications can build upon.** It will subsume eyesim's
basic scientific capabilities through clearer abstractions and more consistent workflows, while
supporting analyses that begin with raw samples, preserve experimental context, and extend beyond
fixation-pattern similarity.

The core should make a simple analysis easy to express and a demanding analysis possible without
leaving its scientific model. A researcher should be able to understand what a result measures,
inspect the choices that produced it, and reuse the analysis with another recording or study.

## Mission

Build and maintain a small set of composable, well-tested abstractions for gaze data, geometry,
time, event detection, spatial measures, scanpath comparison, and experimental design. Make them
available through considerate Scala APIs and inspectable, versioned analysis plans. Supply the
documentation, numerical evidence, and extension contracts needed to use and develop new methods
with confidence.

The mission has three equally necessary outcomes:

- **Scientific completeness:** familiar eyesim workflows are expressible end to end, with explicit
  conventions and documented differences. Breadth beyond that baseline grows through modules.
- **Excellent use:** common tasks read in the language of the analysis, compose predictably, and
  expose meaningful results and errors without repetitive setup.
- **A durable platform:** new methods and a separate analysis application reuse the same domain
  values, execution semantics, persistence contracts, and published laws.

## Relationship to eyesim

eyesim provides a capability baseline and a source of scientific examples. Coverage is measured by
the questions a researcher can answer, not by reproducing its function signatures or internal
structure. Fixation representation, geometry, density and entropy, similarity, template and
repetition designs, multiscale and temporal analysis, and reusable results belong in that baseline.
[The capability map](EYESIM_CAPABILITIES.md) makes the scope and remaining gaps explicit.

For a shared estimand and convention, agreement with a verified eyesim reference is useful
evidence. Confirmed defects become documented divergence tests; they do not become compatibility
requirements. A missing baseline workflow remains a gap even when its underlying primitives
exist. Advanced eyesim models can become optional modules after their shared requirements are
understood; new eyesim features do not silently expand the core release boundary.

## Who the library serves

Researchers are the scientific audience. Scala users should have a satisfying direct API, with
short, runnable guides organized by task. Method developers need small extension interfaces,
published conformance suites, and examples that work outside this repository. Researchers working
in R or Python need useful exports and clear migration guidance; a native language binding is a
separate deliverable.

A future eye-movement analysis UI is an intended consumer, built in another repository. Its needs
inform plan inspection, typed configuration, prerequisites, persistence, result descriptions,
progress, and cancellation. Building or selecting the UI framework is not a prerequisite for
advancing this library. Browser experiment builders remain supported through Scala.js and the
streaming interfaces, without determining the core's entire scope.

## Scientific structure

1. **Geometry and time travel with the data.** Units distinguish pixels from degrees. Runtime
   frame and clock identities distinguish coordinate systems and timelines; `Agreement` is the
   shared compatibility check. Missing geometry or synchronization is reported explicitly.
2. **Trajectory and occupancy remain distinct.** A scanpath preserves order; its induced spatial
   measure discards order explicitly. Sequence methods and distribution methods share useful
   foundations without pretending to answer the same question.
3. **Design is part of the analysis.** Trial identity, matching, control selection, temporal
   windows, reduction, and contrasts are named choices. Results preserve unmatched observations,
   exclusions, failed comparisons, and realized denominators.
4. **Methods state their meaning.** A measure exposes its estimand, scale, conventions, and
   prerequisites. It claims only the mathematical laws it satisfies. Computational correctness
   does not itself establish scientific suitability for every dataset.
5. **Execution follows a description.** Plans describe operations and their dependencies;
   interpreters reuse the same scientific implementations as direct calls. Persistence must
   preserve those choices or report a specific incompatibility.

## What excellent use means

The common workflow should require the scientific decisions, not repeated reconstruction of
frames, clocks, grids, identifiers, and evidence objects. Named configuration values collect
related choices; convenience functions delegate to the same checked operations used by experts.
There must be one understandable route from a task-oriented example to those underlying values.

Usability is an acceptance criterion. The baseline guides must run without private helpers,
unchecked extraction of successful results, unsafe casts, or undocumented setup. Defaults must be
named and inspectable, and choices that change the estimand must remain visible. Results should
make it straightforward to obtain scores, retained keys, diagnostics, and exportable tables.
Error messages should identify the input or operation and explain what would make it admissible.

Examples will cover both existing fixations and raw recordings. A user with trusted fixation
events should not have to invent raw samples or rerun a detector to perform a density comparison.

## What a modular platform means

The kernel stays neutral; eye-specific concepts belong in core and domain modules. Effects remain
in `io` and `fs2`. Specialized reading, saliency, pupillometry, visual-world, and advanced modeling
modules should depend on shared contracts rather than require changes throughout the core.
Heavy numerical backends and rendering integrations remain optional dependencies.

An extension is proven by an example outside the core implementation: a method with typed
parameters and results, its conformance tests, a registered plan description, and a versioned
codec. Adding it must not require a new case in every interpreter. Direct custom functions remain
useful, but persistence of custom behavior requires an explicit registered definition; an opaque
closure is not a saved analysis.

Abstractions earn their place through concrete use. No empty interfaces, speculative plugin
marketplace, or second general trajectory product is needed to establish this foundation.

## Evidence and reproducibility

Every numerical method needs a clear specification, relevant laws or metamorphic properties, and
independent correctness evidence. Evidence may be an analytic case, an exhaustive small-instance
oracle, or a pinned external implementation with its assumptions recorded. Add tests for the
uncertainty or failure mode at hand; neither a fixed artifact quota nor a line-count ratio governs
the work. New law suites must demonstrate that they detect deliberate implementation mutations.

Reproducibility has distinct obligations: input identity, explicit configuration and seed, stable
ordering, algorithm and schema versions, and stated numerical comparison criteria. RNG streams,
canonical identity encodings, and designated golden workflows have exact cross-runtime checks.
Other numerical results require a declared and tested equality or named tolerance contract.
Hash equality for a rounded export does not prove equality of every intermediate floating-point
value. A numerical-tolerance contract never authorizes approximate verification of a content hash.

Smart constructors, operand-bearing errors, no silent cardinality loss, published laws, and the
JVM/Scala.js boundary checks remain core commitments. The system prevents classes of mistakes and
makes other choices auditable; it does not certify an interpretation merely because it compiles.

## Priorities and boundaries

The next substantial advance is a complete, pleasant baseline library plus a demonstrated modular
and persistence boundary. The plan joins scientific completion and usability work with the first
saved analyses, rather than postponing either until an application exists.

Maintain supported importers and fix correctness defects. Prioritize additional vendor depth,
certification infrastructure, and performance machinery when they answer a concrete need. Licensed
EDF verification remains available as a separate evidence programme; it does not block the pure
core or the plan/codec boundary. A redistributable real ASC fixture remains a requirement for
claims of real-data ASC support.

General statistical modeling, acquisition drivers, GUI implementation, and rendering engines are
outside the core. Reusable scientific inputs and outputs for those consumers are in scope.
Release readiness requires completed scientific workflows, usable documentation, external
extension proof, and verified persistence, rather than a count of methods or lines of code.
