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

import eyes4s.codec.SchemaLadder
import eyes4s.plan.DefinitionId
import io.circe.Json
import org.scalacheck.{Gen, Test}

/** Deliberately broken copies of a shipped ladder, rebuilt through the
  * public `SchemaLadder` API, and the check that the published
  * `SchemaLadderLaws` falsify them.
  */
object LadderMutants:
  /** Mutant checks run from a fixed seed, so every kill is reproducible. */
  val parameters: Test.Parameters =
    Test.Parameters.default.withMinSuccessfulTests(60).withInitialSeed(0x4c4144444552L)

  /** The shipped ladder with the upcast into `to` and the vocabulary of
    * `version` replaced where the arguments say so.
    */
  def rebuilt[A](ladder: SchemaLadder[A])(
      upcast: PartialFunction[DefinitionId, Json => Json] = PartialFunction.empty,
      expresses: PartialFunction[DefinitionId, A => Boolean] = PartialFunction.empty
  ): SchemaLadder[A] =
    val first = ladder.versions.head
    ladder.versions.tail.foldLeft(
      SchemaLadder.of[A](ladder.role, first)(ladder.writeAt(first, _))(ladder.readAt(first, _))
    ) { (acc, version) =>
      val previous = acc.latest
      val lift     = upcast.applyOrElse(
        version,
        _ => (json: Json) => ladder.upcast(previous, json).fold(_ => json, _._2)
      )
      val vocabulary = expresses.applyOrElse(
        previous,
        _ => (a: A) => ladder.earliest(a).version <= previous.version
      )
      acc.next(vocabulary, lift)(ladder.writeAt(version, _))(ladder.readAt(version, _))
    }

  /** The properties the ladder laws falsify for `ladder`, by name, sorted. */
  def falsified[A](
      ladder: SchemaLadder[A],
      gen: Gen[A],
      equivalent: (A, A) => Boolean
  ): Vector[String] =
    SchemaLadderLaws
      .ladder(ladder, gen, equivalent)
      .all
      .properties
      .toVector
      .filter((_, prop) =>
        Test.check(parameters, prop).status match
          case Test.Failed(_, _) => true
          case _                 => false
      )
      .map(_._1.stripPrefix("all.").stripPrefix("schemaLadder."))
      .sorted

  /** Every property of the ladder laws passes outright. */
  def passes[A](ladder: SchemaLadder[A], gen: Gen[A], equivalent: (A, A) => Boolean): Boolean =
    SchemaLadderLaws
      .ladder(ladder, gen, equivalent)
      .all
      .properties
      .forall((_, prop) => Test.check(parameters, prop).passed)
