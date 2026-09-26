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

package eyes4s.studio.app.text

import eyes4s.studio.core.freshness.FreshnessText
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

/** The message catalogue and number formatting (ticket S1.0). */
class MessagesSuite extends munit.ScalaCheckSuite:

  test("every id has a non-blank template with placeholders numbered from 0") {
    MessageId.values.foreach { id =>
      val template = Catalogue.english.template(id)
      assert(template.trim.nonEmpty || id == MessageId.PathSeparator, id)
      val slots = Messages.placeholders(template)
      assertEquals(slots, (0 until slots.size).toSet, id)
    }
  }

  test("fill replaces each placeholder once and leaves a missing one visible") {
    assertEquals(Messages.fill("Run {0} · {1}", Vector("8", "Comparing")), "Run 8 · Comparing")
    assertEquals(Messages.fill("{1}{0}{1}", Vector("a", "b")), "bab")
    assertEquals(Messages.fill("Run {0} · {1}", Vector("8")), "Run 8 · {1}")
    assertEquals(Messages.fill("{x} {} {12345}", Vector("a")), "{x} {} {12345}")
    assertEquals(Messages.fill("{0}", Vector("$1 {1}")), "$1 {1}")
  }

  test("counts group in threes with U+2212 for negatives") {
    assertEquals(Format.count(21400L), "21,400")
    assertEquals(Format.count(44845L), "44,845")
    assertEquals(Format.count(7214L), "7,214")
    assertEquals(Format.count(999L), "999")
    assertEquals(Format.count(0L), "0")
    assertEquals(Format.count(-35876L), "−35,876")
    assertEquals(Format.count(Long.MinValue), "−9,223,372,036,854,775,808")
  }

  property("grouping agrees with S2.7's reference except for the minus sign") {
    forAll(Gen.choose(Long.MinValue + 1, Long.MaxValue)) { n =>
      assertEquals(Format.count(n), FreshnessText.count(n).replace("-", Format.Minus))
    }
  }

  test("decimals round half-up; signs use U+2212 and '+'") {
    assertEquals(Format.decimal(0.375, 2), "0.38")
    assertEquals(Format.decimal(-0.08, 2), "−0.08")
    assertEquals(Format.signed(0.38, 2), "+0.38")
    assertEquals(Format.signed(-0.08, 2), "−0.08")
    assertEquals(Format.signed(0.0, 2), "0.00")
    assertEquals(Format.signed(-0.0001, 2), "0.00")
    assertEquals(Format.decimal(Double.NaN, 2), "—")
  }

  test("percent and clock") {
    assertEquals(Format.percent(21400.0 / 44845.0), "48%")
    assertEquals(Format.percent(1.5), "100%")
    assertEquals(Format.percent(Double.NaN), "0%")
    assertEquals(Format.clock(10, 24), "10:24")
    assertEquals(Format.clock(9, 5), "09:05")
  }
