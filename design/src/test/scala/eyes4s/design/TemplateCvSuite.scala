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

package eyes4s.design

import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.compare.Distribution

class TemplateCvSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val CvTolerance                       = 1e-12
  private val grid = get(Grid.over(get(Frame.screen("cv", 2, 1)), 2, 1))
  private def mass(values: Vector[Double]): Mass[Px] =
    val normalized = IArray.from(values.map(_ / values.sum))
    get(Surface.mass(grid, normalized, Provenance.raw(ContentHash.of(normalized))))
  final case class SourceKey(id: String, matched: String) derives CanEqual
  private given KeyDigest[SourceKey] = KeyDigest.derived[SourceKey]
  private val leftMatch              = Projection.named[SourceKey, String]("matched")(_.matched)
  private val rightMatch             = Projection.named[String, String]("matched")(identity)
  private val design = Pairing.between[SourceKey, String].sameOn(leftMatch, rightMatch).all
  private val refs   = Trials(
    TemplateCvReference.references.map((key, values) => Trial(key, (), mass(values)))
  )
  private val observations = TemplateCvReference.rows.map(r =>
    get(
      MapTemplateObservation.of(
        SourceKey(r.key, r.matched),
        r.group,
        r.matched,
        mass(r.values),
        1.0
      )
    )
  )

  test("explicit pinned folds reproduce public template CV scores and training exclusions") {
    TemplateCvReference.folds.foreach { (fold, trainingCount, heldCount, excludedCount) =>
      val heldGroups = TemplateCvReference.rows.filter(_.fold == fold).map(_.group).toSet
      val split      =
        get(MapTemplateSplit.of(observations, heldGroups, "participant", "unused response"))
      assertEquals(split.training.rows.size, trainingCount)
      assertEquals(split.heldOut.rows.size, heldCount)
      assertEquals(split.excluded.size, excludedCount)
      val heldMatches = split.heldOut.rows.map(_.matchGroup).toSet
      assert(split.training.rows.forall(r => !heldMatches(r.matchGroup)))
      val source  = Trials(split.heldOut.rows.map(r => Trial(r.key, (), r.map)))
      val matched = pair(source, refs, design)
      assert(matched.unmatchedLeft.isEmpty && matched.ambiguous.isEmpty)
      matched.pairs.foreach { (left, right) =>
        val score    = get(Distribution.cosine[Px].compare(left.value, right.value)).value
        val expected = TemplateCvReference.rows.find(_.key == left.key.id).get.expected
        assertEqualsDouble(score, expected, CvTolerance)
      }
    }
  }

  test("native matching retains missing source keys and refuses duplicate reference keys") {
    val source  = Trials(observations.map(r => Trial(r.key, (), r.map)))
    val missing = pair(source, Trials(refs.rows.filterNot(_.key == "a")), design)
    assertEquals(missing.unmatchedLeft.map(_.id), Vector("s1"))
    assertEquals(missing.pairs.size, source.size - 1)
    val duplicates =
      pair(source, Trials(refs.rows :+ Trial("a", (), mass(Vector(0, 1)))), design)
    assert(duplicates.ambiguous.nonEmpty)
    assert(!duplicates.pairs.exists(_._1.key.id == "s1"))
  }
