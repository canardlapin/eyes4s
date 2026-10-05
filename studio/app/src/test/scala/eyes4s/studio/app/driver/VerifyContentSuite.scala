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

package eyes4s.studio.app.driver

import eyes4s.studio.app.{AppEffect, StoryModels}
import eyes4s.studio.core.backend.BackendError
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.{AdmissionDecision, DatasetRevisionSpec}
import eyes4s.studio.core.fixture.{StoryMoment, StoryMoments}
import eyes4s.studio.core.headless.HeadlessSession

import scala.concurrent.ExecutionContext

/** The admission request carries the verified content (protocol 1.9, the
  * S5.6 follow-up): Verify sends the digest of what the revision asks eyes4s
  * to admit, and a backend that holds other content for the revision refuses
  * with `ContentMismatch`, naming both, so the app never admits content
  * eyes4s did not see.
  */
class VerifyContentSuite extends munit.FunSuite:

  private given ExecutionContext = ExecutionContext.global

  private val r3 = StoryMoments.r3

  private def ok[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)

  /** t1 with pending r3 sent for verification, its request and its content. */
  private def verifying = ok(
    StudioDriver.open(StoryModels.t1Data).command(Command.VerifyDataset(r3))
  )

  private def requested(d: StudioDriver) =
    d.model.document.dataset(r3).map(_.decision) match
      case Some(AdmissionDecision.Verifying(c)) => c
      case other                                => fail(s"r3 is not verifying: $other")

  private def other = eyes4s.codec.CanonicalDigest
    .parse[DatasetRevisionSpec]("ab" * 32)
    .fold(e => fail(e.message), identity)

  test("the request carries the verified content, and the backend that holds it answers") {
    val d       = verifying
    val content = requested(d)
    assertEquals(
      DatasetRevisionSpec.contentDigest(d.model.document.dataset(r3).get),
      Right(content)
    )
    for
      session <- HeadlessSession.open(StoryMoment.T1)
      _       <- session.holdContent(r3, content)
      settled <- StudioDriver.settle(d, session)
      _       <- session.close
    yield
      assert(
        settled.records.contains(
          DriverRecord.Performed(AppEffect.RequestAdmission(r3, content))
        ),
        settled.records
      )
      assertEquals(settled.admissions.map(_.dataset), Vector(r3))
  }

  test("a backend that holds other content for the revision refuses, naming both digests") {
    val d       = verifying
    val content = requested(d)
    for
      session <- HeadlessSession.open(StoryMoment.T1)
      _       <- session.holdContent(r3, other)
      settled <- StudioDriver.settle(d, session)
      _       <- session.close
    yield
      assertEquals(settled.admissions, Vector.empty)
      assert(
        settled.records.contains(
          DriverRecord.Refused(
            AppEffect.RequestAdmission(r3, content),
            ServiceError.Backend(BackendError.ContentMismatch(r3, content, other))
          )
        ),
        settled.records
      )
      // r3 stays verifying: nothing was admitted on another content's answer.
      assertEquals(
        settled.model.document.dataset(r3).map(_.decision),
        Some(AdmissionDecision.Verifying(content))
      )
  }
