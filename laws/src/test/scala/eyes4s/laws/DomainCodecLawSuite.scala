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
import eyes4s.core.ObservedCoverage
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.*
import eyes4s.plan.DefinitionId
import org.scalacheck.{Gen, Test}

class DomainCodecLawSuite extends munit.DisciplineSuite:
  private def checked[E, A](e: Either[E, A]): A = e.fold(x => fail(s"$x"), identity)
  private val schema                            = checked(DefinitionId.of("test.domain", 1))
  private val frames                            = for
    id   <- Gen.alphaStr
    x    <- Gen.choose(-1000, 1000)
    w    <- Gen.choose(1, 1000)
    axis <- Gen.oneOf(YAxis.values.toSeq)
  yield Frame.of(
    FrameId(id),
    checked(Bounds.of[Px](x.toDouble, -2, (x + w).toDouble, 10)),
    axis
  )
  private val clocks = Gen.alphaStr.map(ClockId.apply)
  private val times  = Gen.oneOf(
    Gen.chooseNum(Long.MinValue, Long.MaxValue),
    Gen.const(9007199254740993L),
    Gen.const(Long.MinValue),
    Gen.const(Long.MaxValue)
  )
  private val intervals = for
    c <- clocks
    a <- times
    b <- times
  yield checked(Interval.of(c, Instant.micros(math.min(a, b)), Instant.micros(math.max(a, b))))
  private val grids = for
    f  <- frames
    nx <- Gen.choose(1, 40)
    ny <- Gen.choose(1, 40)
  yield checked(Grid.of(GridId("grid"), f, nx, ny))
  private def law[A](name: String, codec: VersionedCodec[A], gen: Gen[A]): Unit =
    checkAll(name, CodecLaws.roundTrip(codec, gen, (a: A, b: A) => a == b))

  law("frame", DomainCodecs.frame[Px](schema), frames)
  law("grid", DomainCodecs.grid[Px](schema), grids)
  law("clock", DomainCodecs.clock(schema), clocks)
  law("instant", DomainCodecs.instant(schema), times.map(Instant.micros))
  law("span", DomainCodecs.span(schema), times.map(Span.micros))
  law("interval", DomainCodecs.interval(schema), intervals)
  law(
    "window",
    DomainCodecs.window(schema),
    intervals.map(i =>
      checked(Window.of(Span.micros(i.onset.toMicros), Span.micros(i.offset.toMicros)))
    )
  )
  law(
    "perspective",
    DomainCodecs.perspective(schema),
    Gen.choose(1, 2000).map(n => checked(Perspective.millimetres(n.toDouble, 300, 200)))
  )
  law(
    "sync",
    DomainCodecs.synchronization(schema),
    times.map(n =>
      checked(Sync.affine(ClockId("source"), ClockId("target"), Span.micros(n), 0.0001))
    )
  )
  law(
    "mark",
    DomainCodecs.syncMark(schema),
    times.map(n => checked(SyncMark.of("trigger", Instant.micros(n), Instant.micros(n))))
  )

  checkAll(
    "unit",
    CodecLaws.roundTrip(
      DomainCodecs.unit[Px](schema),
      Gen.const(UnitLabel[Px]),
      (a: UnitLabel[Px], b: UnitLabel[Px]) => a.symbol == b.symbol
    )
  )
  checkAll(
    "coverage",
    CodecLaws.roundTrip(
      DomainCodecs.coverage(schema),
      intervals.map(i =>
        checked(ObservedCoverage.of(i.clock, if i.isEmpty then Vector.empty else Vector(i)))
      ),
      (a: ObservedCoverage, b: ObservedCoverage) =>
        a.clock == b.clock && a.intervals == b.intervals
    )
  )
  private val identityCodec = DomainCodecs.identities(schema)
  checkAll(
    "identities",
    CodecLaws.roundTrip(
      identityCodec,
      grids.map(g => checked(DocumentIdentities.empty.addGrid(g)).addClock(ClockId("a"))),
      (a: DocumentIdentities, b: DocumentIdentities) =>
        identityCodec.encode(a) == identityCodec.encode(b)
    )
  )

  test("published roundtrip laws kill frame ID, y-axis and microsecond-unit mutants") {
    val frameCodec = DomainCodecs.frame[Px](schema)
    def mutant[A](codec: VersionedCodec[A])(change: A => A): VersionedCodec[A] =
      VersionedCodec.checked[A](schema)(a =>
        codec
          .encode(a)
          .flatMap(j =>
            j.hcursor
              .get[io.circe.Json]("value")
              .left
              .map(e => CodecError.Field("value", j, e.message))
          )
      )(raw =>
        codec
          .decode(
            io.circe.Json.obj(
              "schema" -> io.circe.Json.obj(
                "name"    -> io.circe.Json.fromString(schema.name),
                "version" -> io.circe.Json.fromInt(1)
              ),
              "value" -> raw
            )
          )
          .map(change)
      )
    def killed[A](codec: VersionedCodec[A], gen: Gen[A]): Boolean =
      CodecLaws.roundTrip(codec, gen, (a: A, b: A) => a == b).all.properties.exists {
        case (_, prop) =>
          !Test.check(Test.Parameters.default.withMinSuccessfulTests(30), prop).passed
      }
    val fixed = Frame.of(FrameId("original"), checked(Bounds.of[Px](0, 0, 2, 3)), YAxis.Down)
    assert(
      killed(
        mutant(frameCodec)(f => Frame.of(FrameId(f.id.name + "-lost"), f.bounds, f.yAxis)),
        Gen.const(fixed)
      )
    )
    assert(
      killed(mutant(frameCodec)(f => Frame.of(f.id, f.bounds, YAxis.Up)), Gen.const(fixed))
    )
    assert(
      killed(
        mutant(DomainCodecs.instant(schema))(i => Instant.micros(i.toMicros / 1000L)),
        Gen.const(Instant.micros(9007199254740993L))
      )
    )
  }
