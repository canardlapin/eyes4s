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

package eyes4s.io

import eyes4s.kernel.*
import eyes4s.plan.*

class FixationRowInterpretationSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)
  private val frame                             = get(Frame.screen("row-frame", 100, 100))
  private val keys   = get(FixationKeyReader.study("participant", "item", "phase"))
  private val header =
    Vector("participant", "item", "phase", "n", "x", "y", "onset", "duration", "samples")
  private val spec = RowSpec(
    "n",
    "x",
    "y",
    "onset",
    "duration",
    SampleCountRule.PositiveColumn("samples"),
    TimestampUnit.Milliseconds,
    Vector.empty
  )
  private val raw = Vector("p", "i", "encode", "3", "120", "20", "2", "4", "5")
  private def parse(fields: Vector[String], record: Int) =
    FixationCsv.parseRow(
      header,
      fields,
      record,
      spec,
      keys.read,
      keys.clock,
      frame,
      AdmissionPolicy.default[StudyKey],
      keys.participant,
      TimestampRounding.NearestMicrosecond
    )

  test("resumed row interpretation retains absolute source locations and scientific units") {
    val parsed = get(parse(raw, 42))
    assertEquals(parsed.row, 42)
    assertEquals(parsed.raw, raw)
    assertEquals(parsed.key, StudyKey("p", "i", "encode"))
    assertEquals(parsed.ordinal, 3)
    assertEquals(parsed.fixation.centre.x, 120.0)
    assertEquals(parsed.fixation.centre.y, 20.0)
    assertEquals(parsed.fixation.span.onset.toMicros, 2000L)
    assertEquals(parsed.fixation.span.offset.toMicros, 6000L)
    assertEquals(parsed.outside, Some(OutsideFrame(42, 120.0, 20.0, frame.id)))
    assertEquals(parsed.conflict, None)
  }

  test(
    "row refusal retains raw fields and absolute location with key-before-width precedence"
  ) {
    val short = raw.take(1)
    val key   = parse(short, 42).left.toOption.getOrElse(fail("expected key refusal"))
    assertEquals(key.rowNumber, 42)
    assertEquals(key.raw, short)
    assert(key.error.isInstanceOf[FixationRowError.Key])
    val wide  = raw :+ "unexpected"
    val width = parse(wide, 43).left.toOption.getOrElse(fail("expected width refusal"))
    assertEquals(width.rowNumber, 43)
    assertEquals(width.raw, wide)
    assertEquals(width.error, FixationRowError.Width(header.size, wide.size))
    assertEquals(width.key, Some(StudyKey("p", "i", "encode")))
  }
