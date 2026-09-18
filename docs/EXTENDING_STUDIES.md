# Add a comparison without changing the interpreter

The [isolated consumer](../tools/study-consumer) adds a scaled cosine comparison with its own typed
multiplier, score and trial key. It imports packaged eyes4s artifacts; it has no project dependency
on the library's source tree and uses the public `example` package.

A method author supplies:

1. `StudyMethod[P, U, S, D]`: a stable method identity/version, typed parameter description and a
   comparison constructed from `P`, with `ScoreMean[S]` and `Contrastable[S, D]` instances.
2. A `StudyLayout[K]` defining participant, stimulus and phase projections, with a stable layout
   identity, `KeyDigest[K]`, and a lawful key ordering.
3. Conditional `VersionedCodec[K]` and `VersionedCodec[P]` instances. `StudyCodec` captures these
   with the method before registration; runtime lookup never reconstructs their types by casting.
4. `ScoreColumns[S, D]` if tabular output is needed. Export rejects the wrong component count or
   non-finite projected values instead of producing malformed or misleading rows.

The example runs the published `CodecLaws` against custom parameters, custom keys and complete plans,
and `ContrastLaws` against its custom score. It then registers, saves, reloads, executes and exports a
three-scale study. Missing methods, duplicate registration, invalid parameters and missing schemas
are explicit failures. The method can be removed from the registry without modifying its interpreter.

## Spatial scales

`StudyEstimate.Gaussian(sigma, edges)` carries a standard deviation in frame units and an explicit
edge policy. Each scale retains its own estimates, failures and contrasts. Scales are not averaged
or silently dropped. Plans preserve scale order and reject duplicate scale declarations.

The [independent oracle](../tools/r-parity/generate_multiscale.py) evaluates the two-by-two fixture in
60-digit decimal arithmetic. With `t = exp(-1 / (2 sigma^2))`, a cell with mass count `a`, its two
neighbors `b,c`, and diagonal `d` has normalized mass
`(a + t(b+c) + t^2 d) / (total (1+t)^2)`. This closed form needs no convolution implementation.
It supplies 36 maps and 18 contrasts at sigma 0.5, 1 and 2 pixels.

All four cells in this fixture are corners. Source edge correction is consequently constant and
cancels on normalization, so both edge policies share these particular normalized targets. This
fixture does not establish general equivalence of the edge policies or parity with an R KDE.

The consumer's positive multiplier has an independent prediction: every contrast is multiplied by
that value at every scale. This checks the extension's parameter persistence and execution together.
The application-facing result remains typed; adding a GUI belongs in its separate repository.

Run `python3 tools/study-consumer/verify.py` from the repository root to publish the development
artifacts locally and execute the consumer in a fresh directory. The verifier checks both
classpaths and compares emitted JVM/Scala.js numerical results, keeping logs and a receipt in the
printed directory. Input digests, decoded plan values and binned result bits must match exactly;
Gaussian results use the named `1e-12` absolute tolerance. The fixture generator's `--check` also
checks the consumer's copied input and target data.

## Add a detector with typed parameters

The consumer's laboratory detector (`CustomDetector`) wraps the canonical I-VT machine with its own
parameter type. Each field is a `ParameterDescriptor` that constructs the library's typed value
through the library's smart constructors, with the extension's own error type; `bind` states how the
field is read from the parameters and recorded in provenance. The descriptor names the algorithm
card of the machine it wraps, which is the card every detection it produces carries. The excerpt
below omits `minimumField` (the same shape for microseconds), `detector` and `parameterCodec`; the
complete source is
[`CustomDetector.scala`](../tools/study-consumer/src/main/scala/example/CustomDetector.scala):

```scala
def thresholdField: Either[DescriptorError, ParameterDescriptor[Double, IvtThreshold, String]] =
  ParameterInfo
    .of("thresholdDegPerSecond", 1, "Conservative I-VT velocity threshold on the angular samples",
      ParameterUnits.PerSecond("deg"), ParameterDomain.PositiveFinite)
    .map(info => new ParameterDescriptor(info, LabIvtParameters.threshold, identity))

def descriptor(id: DefinitionId): Either[DescriptorError, RecordingMethodDescriptor[LabIvtParameters]] =
  for
    threshold <- thresholdField
    minimum   <- minimumField
    fields <- ParameterSet.of(Vector(
      threshold.bind[LabIvtParameters](_.threshold)(t => Provenance.Param.Num(t.velocity.value)),
      minimum.bind[LabIvtParameters](_.minimumDuration)(m =>
        Provenance.Param.Text(m.span.toMicros.toString))))
  yield new RecordingMethodDescriptor(id, fields, AlgorithmCards.ivt)

def describedMethod(id: DefinitionId): Either[DescriptorError, RecordingMethod[LabIvtParameters]] =
  descriptor(id).map(d => new RecordingMethod(id, d.parameters.values, detector, Some(d)))

def persistence(schema: DefinitionId, methodId: DefinitionId, parameterSchema: DefinitionId)
    : Either[DescriptorError, RecordingPlanCodec[LabIvtParameters]] =
  describedMethod(methodId).map(new RecordingPlanCodec(schema, _, parameterCodec(parameterSchema)))
```

The plan codec's `results` is its `recording-result@1` archive. An application registers both, and
nothing else, before it resolves a saved recording; `withRecordings` takes the pixel witness, so a
manifest in any other unit cannot register a recording plan (`RecordingRoute.decoders`):

```scala
for
  base     <- ArtifactDecoders.study[Px]
  plans    <- RecordingRegistry.empty.register(persistence.registration)
  archived <- RecordingResultRegistry.empty.register(persistence.results.registration)
yield base.withRecordings(plans, archived)
```

The described detector then behaves like a shipped one: `plan.inspect` lists its fields under
`detector.`, preflight checks it (an undescribed method is a warning, never a blocker),
`RecordingExecution[IO]` runs it in sample chunks with progress and cancellation, and
`RecordingJourney.save` archives its analysis under the recording roles. A saved analysis of an
unregistered detector is refused by schema (`UnsupportedSchema`) and by method
(`MissingResultCodec`), never guessed. `ConsumerLawsSuite` runs the published `ExecutionLaws` and
`ManifestLaws` over it. A new detector algorithm, rather than a new parameterization of an existing
machine, would also supply its own machine and a truthful algorithm card.

## Run your comparison over temporal windows

A temporal plan runs a base study in every window of every repetition, so an extension's comparison,
keys and scores carry over unchanged. `TemporalRoute` builds the temporal codecs from a fixation
route: the plan codec over the route's `StudyCodec`, the temporal input with its base study stored
by reference beside it, and the `temporal-result@1` archive with the route's own score codecs:

```scala
val persistence = new TemporalStudyCodec(schema, route.persistence)
val inputs = new TemporalInputCodec(TemporalInputCodecs.input, route.inputs,
  StudyEmbedding.ByReference, TemporalInputCodecs.unresolved[K, Px])
val results = persistence.results(route.results.scores, route.results.differences)

for
  base     <- route.decoders
  plans    <- TemporalRegistry.empty[K, Px].register(persistence.registration)
  archived <- TemporalResultRegistry.empty[K, Px].register(results.registration)
yield base.withTemporal(plans, archived)
```

The consumer's scaled cosine runs this way over `TrialKey`: every temporal contrast is the shipped
cosine's doubled, bit for bit at the binned scale, and every ledger and contrast agrees with the
independent temporal oracle. The temporal plan's inspection states the base method's execution
capability, since every cell steps the base study's cursor.

## What an application can rely on, and where it stops

`tools/study-consumer` is the headless reference application for all three shipped plan families:
[its README](../tools/study-consumer/README.md) walks through each journey, the failure paths it
pins, the laws it runs over its own types, the response envelope it measures and the receipt
`verify.py` writes. Its limits are listed there: io import errors without diagnostic codes, the
abstract types of a resolved plan, no partial-failure fixture, `PreflightError` without a key type,
quadratic detection support assembly, ledgers not re-verified against their source, unpacked
binocular inputs, no realistic-size input, and no bit-for-bit re-execution of JVM archives of
trigonometric or Gaussian results on Scala.js.
