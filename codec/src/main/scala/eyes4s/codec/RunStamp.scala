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

import eyes4s.plan.PlanChange

/** The canonical documents that produced a run, with distinct plan/input types.
  * Construction hashes both complete versioned documents; a human-readable
  * plan description and a legacy ArtifactRef cannot substitute for either.
  * A stamp is an identity claim, not evidence that an arbitrary result ran it.
  */
final class RunStamp[Plan, Input] private[codec] (
    val plan: CanonicalDigest[Plan],
    val input: CanonicalDigest[Input]
):
  /** Refuse a changed document even if its human-readable diff is empty.
    * When both changed, report the plan first, then the input after it is resolved.
    */
  def check(
      current: RunStamp[Plan, Input],
      changes: Vector[PlanChange]
  ): Either[RunStampError[Plan, Input], Unit] =
    if !plan.sameAs(current.plan) then
      Left(RunStampError.ChangedPlan(plan, current.plan, changes))
    else if !input.sameAs(current.input) then
      Left(RunStampError.ChangedInput(input, current.input))
    else Right(())

object RunStamp:
  def of[Plan, Input](
      plan: Plan,
      input: Input,
      plans: VersionedCodec[Plan],
      inputs: VersionedCodec[Input]
  ): Either[CodecError, RunStamp[Plan, Input]] =
    for
      p <- plans.digest(plan)
      i <- inputs.digest(input)
    yield new RunStamp(p, i)

/** Located stale-run refusals retain both canonical identities. */
enum RunStampError[Plan, Input]:
  case ChangedPlan(
      reported: CanonicalDigest[Plan],
      current: CanonicalDigest[Plan],
      changes: Vector[PlanChange]
  )
  case ChangedInput(reported: CanonicalDigest[Input], current: CanonicalDigest[Input])

  def message: String = this match
    case ChangedPlan(reported, current, changes) =>
      s"Run plan ${reported.display} differs from current ${current.display}; changed fields: ${changes.map(_.field).mkString(", ")}."
    case ChangedInput(reported, current) =>
      s"Run input ${reported.display} differs from current ${current.display}."

/** The persisted claim uses named members; their positions never identify their roles. */
private[codec] object RunStampWire:
  def write[P, I](stamp: RunStamp[P, I]): io.circe.Json = io.circe.Json.obj(
    "plan"  -> io.circe.Json.fromString(stamp.plan.sha256.hex),
    "input" -> io.circe.Json.fromString(stamp.input.sha256.hex)
  )

  def read[P, I](json: io.circe.Json): Either[CodecError, RunStamp[P, I]] =
    def digest[A](field: String): Either[CodecError, CanonicalDigest[A]] =
      Wire
        .field[String](json, field)
        .flatMap(raw =>
          CanonicalDigest
            .parse[A](raw)
            .left
            .map(error => CodecError.Field(field, json, error.message))
        )
    for
      plan  <- digest[P]("plan")
      input <- digest[I]("input")
    yield new RunStamp(plan, input)
