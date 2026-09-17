# Your first study

This five-minute example asks: **is each recall map closer to its matching encoding map than
to the other image's encoding map, within the same participant?** The tiny fixation table is
synthetic, with one fixation per trial; it demonstrates the workflow, not a population effect.
For an acquired recording, follow [the real-data guide](recordings.md).

## Run from source

Use a supported JDK (17 or 21), sbt, and Scala 3.7.4. From the checkout:

```sh
sbt docs/tlSite
```

This executes every example in this public guide. For a downstream build, publish a local
candidate with `sbt 'set ThisBuild / version := "0.0.0-local"' publishLocal`, then use
`"io.github.canardlapin" %% "eyes4s-io" % "0.0.0-local"` on JVM (`%%%` with sbt-scalajs on JS).
This is a local development route, not a promise that those coordinates exist on Maven Central.

## Admit, compare, save and export

The CSV declares pixels and microseconds explicitly. `sample_count` is supplied fixation-summary
support; no raw recording is manufactured from it. The admission report is available before
`requireComplete` rejects any incomplete trial set.

```scala mdoc:silent
import eyes4s.codec.*
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.io.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*

val csv = """participant,image,phase,fixation,x_px,y_px,onset_us,duration_us,sample_count
p1,a,encode,0,0.5,0.5,0,100000,1
p1,b,encode,0,1.5,0.5,0,100000,1
p1,a,recall,0,0.5,0.5,0,100000,1
p1,b,recall,0,1.5,0.5,0,100000,1
"""

val study = for
  frame <- Frame.screen("example", 2, 1)
  grid <- Grid.over(frame, 2, 1)
  columns <- FixationColumns.of("fixation", "x_px", "y_px", "onset_us", "duration_us", "sample_count")
  keys <- FixationKeyReader.study("participant", "image", "phase")
  admitted <- FixationCsv.read(csv, columns, keys, frame, TimestampUnit.Microseconds)
  input <- admitted.requireComplete
  plan <- StudyPlan.cosine(input.reference, grid, "recall", "encode", Weight.Duration,
    Vector(StudyEstimate.Binned()), FailurePolicy.RequireAll)
  persistence = StudyCodecs.cosine[Px]
  json <- persistence.codec.encode(plan)
  restored <- persistence.codec.parse(json.noSpaces)
  result <- restored.run(input)
  table <- ContrastCsv.document(restored, result, persistence, ScoreColumns.similarity)
yield (plan, restored, table)

assert(study.exists { case (plan, restored, table) =>
  plan.diff(restored).isEmpty && table.rows.size == 2
})
```

The result is still an `Either`: a real application handles a typed error, rather than calling
`.get` or silently dropping a trial.

```scala mdoc
study.map { case (_, _, table) => table.rows.size }
```

For this constructed case the matching cosine is 1, the control cosine is 0, and their difference
is 1. Inspect these actual exported values:

```scala mdoc
study.map { case (_, _, table) =>
  table.rows.map(row => ContrastCsv.header.zip(row).toMap.apply("difference"))
}
```

Save `json.noSpaces` to persist the plan and `table.encode` for CSV. The plan refers to the admitted
input by content identity; restore it against the same input, not an unrelated table with the same
filename. Change `Weight.Duration` to `Weight.Uniform` only if each fixation should contribute
equally. Gaussian estimates additionally require an explicit bandwidth and edge policy.

Next: [the mental model](concepts.md), [study choices](fixation-studies.md), or
[an acquired raw recording](recordings.md).
