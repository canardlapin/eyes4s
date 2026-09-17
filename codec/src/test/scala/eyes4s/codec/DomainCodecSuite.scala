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

package eyes4s.codec

import eyes4s.core.ObservedCoverage
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.*
import eyes4s.plan.DefinitionId
import io.circe.Json

class DomainCodecSuite extends munit.FunSuite:
  private def checked[E, A](e: Either[E, A]): A = e.fold(x => fail(s"$x"), identity)
  private val schema                            = checked(DefinitionId.of("test.domain", 1))
  private def frame(id: String, axis: YAxis = YAxis.Down): Frame[Px] =
    Frame.of(FrameId(id), checked(Bounds.of[Px](0, 0, 800, 600)), axis)
  private def envelope(value: Json): Json =
    Json.obj("schema" -> Wire.id(schema), "value" -> value)

  test("all signed 64-bit times, including beyond JS integer precision, remain exact") {
    val codec = DomainCodecs.instant(schema)
    Vector(Long.MinValue, -9007199254740993L, 0L, 9007199254740993L, Long.MaxValue).foreach {
      n =>
        val encoded = checked(codec.encode(Instant.micros(n)))
        assertEquals(checked(encoded.hcursor.downField("value").as[String]), n.toString)
        assertEquals(checked(codec.parse(encoded.noSpaces)).toMicros, n)
    }
    Vector(
      Json.fromLong(1L),
      Json.fromString("1.0"),
      Json.fromString("1e3"),
      Json.fromString("9223372036854775808"),
      Json.Null
    ).foreach { raw =>
      assert(codec.decode(envelope(raw)).isLeft)
    }
  }

  test(
    "geometry rejects malformed dimensions, nonfinite values, wrong units and unknown axes"
  ) {
    val codec = DomainCodecs.frame[Px](schema)
    val raw   = DomainWire.frame(frame("screen"))
    Vector(
      "xMax"  -> Json.fromInt(0),
      "xMax"  -> Json.fromString("NaN"),
      "xMax"  -> Json.Null,
      "unit"  -> Json.fromString("deg"),
      "yAxis" -> Json.fromString("sideways")
    ).foreach { case (k, v) =>
      assert(codec.decode(envelope(raw.mapObject(_.add(k, v)))).isLeft)
    }
    val overflow = Json.obj(
      "id"    -> Json.fromString("grid"),
      "frame" -> raw,
      "nx"    -> Json.fromInt(65536),
      "ny"    -> Json.fromInt(65536)
    )
    assert(DomainCodecs.grid[Px](schema).decode(envelope(overflow)).isLeft)
    assert(
      DomainCodecs
        .grid[Px](schema)
        .decode(envelope(overflow.mapObject(_.add("nx", Json.fromInt(0)))))
        .isLeft
    )
    assert(
      codec
        .decode(
          envelope(raw.mapObject(_.add("xMax", checked(io.circe.parser.parse("1e9999")))))
        )
        .isLeft
    )
  }

  test(
    "document identity tables share nominal references without merging lookalike frames or clocks"
  ) {
    val first  = frame("first")
    val second = frame("second")
    val grid   = checked(Grid.of(GridId("g"), first, 2, 3))
    val table  = checked(checked(DocumentIdentities.empty.addGrid(grid)).addFrame(second))
      .addClock(ClockId("tracker"))
      .addClock(ClockId("stimulus"))
    val codec    = DomainCodecs.identities(schema)
    val restored = checked(codec.parse(checked(codec.encode(table)).noSpaces))
    assertEquals(
      checked(restored.grid[Px](GridId("g"))).frame,
      checked(restored.frame[Px](first.id))
    )
    assert(
      Agreement
        .frames(checked(restored.frame[Px](first.id)), checked(restored.frame[Px](second.id)))
        .isLeft
    )
    assert(
      Agreement
        .clocks(
          checked(restored.clock(ClockId("tracker"))),
          checked(restored.clock(ClockId("stimulus")))
        )
        .isLeft
    )
    assert(restored.frame[Deg](first.id).isLeft)
    assert(restored.clock(ClockId("absent")).isLeft)
    assert(restored.grid[Px](GridId("absent")).isLeft)
    assertEquals(
      checked(codec.encode(checked(table.addFrame(first)))),
      checked(codec.encode(table))
    )
    assert(table.addFrame(frame("first", YAxis.Up)).isLeft)
    assert(table.addGrid(checked(Grid.of(GridId("g"), first, 3, 2))).isLeft)
    val angular =
      Frame.of(FrameId("first"), checked(Bounds.of[Deg](0, 0, 800, 600)), YAxis.Down)
    assert(table.addFrame(angular).isLeft)
  }

  test(
    "document decoder refuses conflicts and unresolved references even in a well formed envelope"
  ) {
    val f    = DomainWire.frame(frame("screen"))
    val grid = Json.obj(
      "id"    -> Json.fromString("g"),
      "frame" -> Json.fromString("absent"),
      "nx"    -> Json.fromInt(2),
      "ny"    -> Json.fromInt(2)
    )
    val codec                                           = DomainCodecs.identities(schema)
    def table(fs: Vector[Json], gs: Vector[Json]): Json = envelope(
      Json.obj("frames" -> Json.arr(fs*), "grids" -> Json.arr(gs*), "clocks" -> Json.arr())
    )
    assert(codec.decode(table(Vector(f), Vector(grid))).left.exists {
      case CodecError.MissingIdentity("frame", "absent") => true
      case _                                             => false
    })
    assert(
      codec
        .decode(
          table(Vector(f, f.mapObject(_.add("yAxis", Json.fromString("Up")))), Vector.empty)
        )
        .isLeft
    )
    assert(
      codec
        .decode(
          table(Vector(f, f.mapObject(_.add("unit", Json.fromString("deg")))), Vector.empty)
        )
        .isLeft
    )
  }

  test(
    "intervals and coverage retain clocks, half-open boundaries and gaps; invalid coverage fails"
  ) {
    val c = ClockId("tracker")
    val a = checked(
      Interval.of(c, Instant.micros(9007199254740993L), Instant.micros(9007199254741003L))
    )
    val b = checked(
      Interval.of(c, Instant.micros(9007199254741013L), Instant.micros(9007199254741023L))
    )
    val coverage = checked(ObservedCoverage.of(c, Vector(b, a)))
    val codec    = DomainCodecs.coverage(schema)
    val decoded  = checked(codec.decode(checked(codec.encode(coverage))))
    assertEquals(decoded.intervals, Vector(a, b))
    assert(!decoded.intervals.head.contains(a.offset))
    val raw = Json.obj(
      "clock"     -> Json.fromString("wrong"),
      "intervals" -> Json.arr(DomainWire.interval(a))
    )
    assert(codec.decode(envelope(raw)).isLeft)
    assert(
      codec
        .decode(
          envelope(
            raw.mapObject(
              _.add("clock", Json.fromString(c.name))
                .add("intervals", Json.arr(DomainWire.interval(a), DomainWire.interval(a)))
            )
          )
        )
        .isLeft
    )
    assert(
      DomainCodecs
        .interval(schema)
        .decode(
          envelope(
            DomainWire
              .interval(a)
              .mapObject(_.add("offsetMicros", Json.fromString("0")))
          )
        )
        .isLeft
    )
  }

  test("perspective and synchronization reconstruct through domain smart constructors") {
    val sync =
      checked(Sync.affine(ClockId("a"), ClockId("b"), Span.micros(9007199254740993L), 0.25))
    val codec = DomainCodecs.synchronization(schema)
    assertEquals(checked(codec.decode(checked(codec.encode(sync)))), sync)
    val raw = checked(codec.encode(sync)).hcursor.get[Json]("value").toOption.get
    assert(codec.decode(envelope(raw.mapObject(_.add("drift", Json.fromInt(-1))))).isLeft)
    assert(
      DomainCodecs
        .perspective(schema)
        .decode(
          envelope(
            Json.obj(
              "distanceMm" -> Json.fromInt(0),
              "widthMm"    -> Json.fromInt(300),
              "heightMm"   -> Json.fromInt(200)
            )
          )
        )
        .isLeft
    )
    assert(
      DomainCodecs
        .syncMark(schema)
        .decode(
          envelope(
            Json.obj(
              "id"           -> Json.fromString(""),
              "sourceMicros" -> Json.fromString("0"),
              "targetMicros" -> Json.fromString("1")
            )
          )
        )
        .isLeft
    )
  }
