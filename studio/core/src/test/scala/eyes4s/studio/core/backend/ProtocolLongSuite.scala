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

package eyes4s.studio.core.backend

import io.circe.{Decoder, Encoder, Json}
import io.circe.parser.decode
import io.circe.syntax.*
import eyes4s.studio.core.execution.{Meter, MeterTotal}
import eyes4s.studio.core.preview.{PreviewCandidates, PreviewCounts, PreviewId}

class ProtocolLongSuite extends munit.FunSuite:
  private val counts =
    Vector(9007199254740991L, 9007199254740992L, 9007199254740993L, Long.MaxValue)

  private def right[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)

  private def roundTrip[A: Encoder: Decoder](value: A): Unit =
    assertEquals(decode[A](value.asJson.noSpaces), Right(value))

  counts.foreach { n =>
    test(s"progress counts survive JSON text at $n") {
      Vector(
        ProgressTotal.Unknown,
        ProgressTotal.Counting,
        ProgressTotal.Exact(n),
        ProgressTotal.AtMost(n)
      ).foreach { total =>
        val meter    = right(StageMeter.of(StageKind.Comparing, CountUnit.Pairs, n, total))
        val totals   = right(RunTotals.of(n, total, n, total))
        val progress = right(
          JobProgress.of(
            JobId(1),
            RunId(2),
            n,
            Segment.Comparing(0, PairDesign.Matched),
            meter,
            totals
          )
        )
        roundTrip(meter)
        roundTrip(totals)
        roundTrip(progress)
        val envelope = Envelope(RequestId(n), ServerFrame.Event(JobEvent.Advanced(progress)))
        assertEquals(WireFormat.parse[ServerFrame](envelope.asJson.noSpaces), Right(envelope))
        assertEquals(
          meter.asJson.hcursor.downField("done").focus,
          Some(if n == counts.head then Json.fromLong(n) else Json.fromString(n.toString))
        )
      }
    }

    test(s"related counts and identities survive JSON text at $n") {
      roundTrip(
        PreviewSummary(
          AnalysisRevision(1),
          DatasetRevision(2),
          Vector("2°"),
          1,
          1,
          1,
          1,
          n,
          n,
          n
        )
      )
      roundTrip(ProtocolSamples.result.copy(pairRowsPerScale = n, pairRows = n))
      roundTrip(WindowTotals(1, 1, 2, 1, 1, 1, 0, None, n, n, n))
      roundTrip(DiagnosticLocus.Line("fixture.asc", n): DiagnosticLocus)
      roundTrip(RequestId(n))
      roundTrip(PreviewId(n))
      roundTrip(right(PreviewCandidates.of(1, 1, 1, n, 1, 0, Some(0))))
      roundTrip(right(PreviewCounts.of(n, n, 0, 0, 0)))
      roundTrip(Meter(CountUnit.Pairs, n, MeterTotal.Exact(n)))
      roundTrip(Meter(CountUnit.Pairs, n, MeterTotal.AtMost(n)))
    }
  }

  test("unsafe numeric text is refused consistently instead of rounded") {
    counts.tail.foreach { n =>
      val wire = ProtocolSamples.progress.meter.asJson
        .deepMerge(
          Json.obj(
            "done"  -> Json.fromLong(n),
            "total" -> (ProgressTotal.Unknown: ProgressTotal).asJson
          )
        )
        .noSpaces
      val failure = decode[StageMeter](wire).swap.toOption.getOrElse(fail(wire))
      assert(failure.getMessage.contains("safe integer"), failure.getMessage)
      assert(failure.getMessage.contains("done"), failure.getMessage)
    }
  }

  test("invalid strings, fractional values and Long overflow are refused") {
    val invalid = Vector(
      Json.fromString("01"),
      Json.fromString("+1"),
      Json.fromString(" 1"),
      Json.fromString("1e3"),
      Json.fromString("9223372036854775808"),
      Json.fromString("-9223372036854775809"),
      Json.fromString("-0"),
      Json.fromDoubleOrNull(1.5),
      Json.Null
    )
    invalid.foreach { value =>
      val wire =
        ProtocolSamples.progress.meter.asJson.deepMerge(Json.obj("done" -> value)).noSpaces
      assert(decode[StageMeter](wire).isLeft, wire)
    }
  }

  test("wire parsing preserves fractional numeric text before count validation") {
    Vector("9007199254740991.1", "1.0000000000000001", "1e-400").foreach { raw =>
      val meter =
        s"""{"kind":{"Comparing":{}},"unit":{"Pairs":{}},"done":$raw,"total":{"Unknown":{}}}"""
      val wire = s"""{"version":{"major":1,"minor":8},"id":1,"body":$meter}"""
      assert(WireFormat.parse[StageMeter](wire).isLeft, wire)
      assertEquals(WireFormat.requestId(s"""{"id":$raw}"""), None)
    }
    val safe =
      s"""{"version":{"major":1,"minor":8},"id":1,"body":${ProtocolSamples.progress.meter.asJson.noSpaces}}"""
    assertEquals(
      WireFormat.parse[StageMeter](safe).map(_.body),
      Right(ProtocolSamples.progress.meter)
    )
  }

  test("negative numeric and string progress still reaches domain refusal") {
    Vector(Json.fromLong(-1), Json.fromString("-1"), Json.fromString(Long.MinValue.toString))
      .foreach { value =>
        val wire =
          ProtocolSamples.progress.meter.asJson.deepMerge(Json.obj("done" -> value)).noSpaces
        val failure = decode[StageMeter](wire).swap.toOption.getOrElse(fail(wire))
        assert(failure.getMessage.contains("negative"), failure.getMessage)
      }
    roundTrip(RequestId(Long.MinValue))
  }

  test("safe numeric 1.2 envelopes remain readable by the current protocol") {
    val envelope = Envelope(
      ProtocolVersion(1, 2),
      RequestId(41),
      ServerFrame.Event(JobEvent.Advanced(ProtocolSamples.progress))
    )
    assertEquals(WireFormat.parse[ServerFrame](envelope.asJson.noSpaces), Right(envelope))
    assertEquals(ProtocolVersion.Current, ProtocolVersion(1, 18))
  }

  test("safe Long inputs accept both compatibility forms and re-encode as canonical numbers") {
    Vector(-5L, 0L, 5L, 9007199254740991L).foreach { n =>
      Vector(Json.fromLong(n), Json.fromString(n.toString)).foreach { input =>
        val id = right(input.as[RequestId])
        assertEquals(id, RequestId(n))
        assertEquals(id.asJson, Json.fromLong(n))
      }
    }
  }
