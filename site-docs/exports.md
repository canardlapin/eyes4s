# Export results without losing failures

Use `ResultExports` to derive checked tables from pair scores, reductions, contrasts,
repetition, point sampling, template fits, surface OLS, and saved spatial or temporal studies.
These adapters retain the original recipe and source identities. The
[complete executable example](https://github.com/canardlapin/eyes4s/blob/main/io/src/test/scala/eyes4s/examples/BaselineExportGuide.scala)
covers each result family with asserted numerical outputs.

Every `ResultTable` has typed columns, explicit units, context and a content identity. The table
layer itself is the pure `eyes4s-results` module, which also renders
[reports](summaries.md); `eyes4s-io` adds this circe entry point and the CSV and Arrow writers.
CSV uses a validity column for each nullable field, so an empty string cannot impersonate
missing data. Preserve the metadata JSON beside the CSV. An archive for replay is a separate artifact.

```scala mdoc:silent
import eyes4s.io.*
import io.circe.Json

val columns = Vector(
  ResultColumn("trial", ResultColumnType.Utf8, false, "key", "Original trial identity"),
  ResultColumn("value", ResultColumnType.Float64, true, "similarity", "Observed score")
)
val exported = ResultTable.of(ResultFamily.PairScores, columns,
  Vector(Vector(ResultCell.Text("a"), ResultCell.Number(0.75)),
         Vector(ResultCell.Text("b"), ResultCell.Missing)),
  Json.obj("example" -> Json.fromString("explicit missing value")))
assert(exported.exists(_.csv.rows.size == 2))
assert(exported.exists(_.csv.rows.last.takeRight(2) == Vector("", "false")))
assert(ResultTable.of(ResultFamily.PairScores, columns,
  Vector(Vector(ResultCell.Text("bad"), ResultCell.Number(Double.NaN))), Json.obj()).isLeft)
```

```scala mdoc
exported.map(_.csv.encode)
```

On JVM, `ArrowResultExport.write` writes the same checked table as Arrow IPC with native validity
and schema metadata. Its optional Arrow dependencies need the documented JVM `--add-opens` setting.
A failed write can leave a partial destination: publish the destination only after a successful result.
The [transport guide](https://github.com/canardlapin/eyes4s/blob/main/docs/RESULT_EXPORTS.md)
includes the independent R and PyArrow read-back commands and qualifications.
