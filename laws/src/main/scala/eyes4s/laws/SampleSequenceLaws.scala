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

import eyes4s.core.Sample
import eyes4s.kernel.*

import org.scalacheck.Gen
import org.scalacheck.Prop
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

/** Timestamp projection used by sample-sequence laws. */
trait Timestamped[-A]:
  def timestamp(value: A): Instant

object Timestamped:
  def apply[A](f: A => Instant): Timestamped[A] =
    new Timestamped[A]:
      def timestamp(value: A): Instant = f(value)

  given [U <: Unit2D]: Timestamped[Sample[U]] = Timestamped(_.t)

/** Laws for transformations that claim to preserve one output per input. */
trait SampleSequenceLaws extends Laws:

  def exactTimestamps[A, B](
      input: Gen[List[A]],
      transform: Machine[A, B]
  )(using inputTime: Timestamped[A], outputTime: Timestamped[B]): RuleSet =
    new SimpleRuleSet(
      "sampleSequence.exactTimestamps",
      "cardinality is unchanged" -> forAll(input) { values =>
        Prop(transform.runAll(values).length == values.length)
      },
      "timestamp sequence is unchanged" -> forAll(input) { values =>
        val expected = values.map(inputTime.timestamp)
        val observed = transform.runAll(values).map(outputTime.timestamp).toList
        Prop(observed == expected)
      }
    )

end SampleSequenceLaws

object SampleSequenceLaws extends SampleSequenceLaws
