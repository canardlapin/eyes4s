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

package eyes4s.studio.core.real

import cats.effect.IO
import eyes4s.codec.ByteDigest
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.{InitialFixationChoice, SourceRole, Sources}
import eyes4s.studio.core.fixture.{GoldenAssets, GoldenCsv, StoryMoments}
import eyes4s.studio.core.navigation.NavigationError
import munit.CatsEffectSuite
import java.nio.charset.StandardCharsets

class FixationContextSuite extends CatsEffectSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private lazy val document                     = get(RealBackendConformanceSuite.trialLayout)
  private lazy val source = get(document.dataset(StoryMoments.r3).toRight("no data"))
  private lazy val recipe = get(
    document.analysis(StoryMoments.rev4).toRight("no analysis")
  ).recipe
  private def prepare(
      revision: AnalysisRevision,
      alternate: Boolean,
      dropFirst: Boolean = false
  ): RealPrepared =
    val text =
      if alternate then GoldenCsv.fixations.replace("\n", "\r\n") else GoldenCsv.fixations
    val spec = if !alternate then source
    else
      source.copy(
        id = DatasetRevision(4),
        sources = get(Sources.of(source.sources.entries.map { entry =>
          if entry.role == SourceRole.Fixations then
            entry.copy(bytes =
              ByteDigest.sha256(IArray.from(text.getBytes(StandardCharsets.UTF_8)))
            )
          else entry
        }))
      )
    val admitted = get(
      RealAdmission.admit(spec, text, GoldenCsv.trials, get(GoldenAssets.registry(spec)))
    )
    get(
      RealPrepared.of(
        revision,
        spec.id,
        if dropFirst then recipe.copy(initialFixations = InitialFixationChoice.DropFirst)
        else recipe,
        admitted
      )
    )

  test(
    "unqualified fixation lookup requires served context and refuses conflicting source identities"
  ) {
    val first       = prepare(AnalysisRevision(4), alternate = false)
    val sameSource  = prepare(AnalysisRevision(5), alternate = false, dropFirst = true)
    val otherSource = prepare(AnalysisRevision(6), alternate = true)
    val views       = get(RealTrialViews.of(first))
    val trial       = first.admitted.ledger
      .find(_.disposition == TrialDisposition.Admitted)
      .getOrElse(fail("no trial"))
      .trial
    val fixation =
      get(views.fixations(trial)).fixations.headOption.getOrElse(fail("no fixation")).ref
    val preparations = Map(
      first.revision       -> first,
      sameSource.revision  -> sameSource,
      otherSource.revision -> otherSource
    )
    for
      state <- RealNavigator.empty[IO]
      navigator = new RealNavigator[IO](
        Map.empty,
        run => IO.pure(Left(BackendError.UnknownRun(run, Vector.empty))),
        revision =>
          IO.pure(
            preparations
              .get(revision)
              .toRight(BackendError.UnknownRevision(revision, preparations.keys.toVector))
          ),
        state
      )
      missing    <- navigator.record(fixation)
      _          <- navigator.rememberTrial(first.revision, Vector(fixation)).map(get)
      original   <- navigator.record(fixation)
      _          <- navigator.rememberTrial(sameSource.revision, Vector(fixation)).map(get)
      equivalent <- navigator.record(fixation)
      _          <- navigator.rememberTrial(otherSource.revision, Vector(fixation)).map(get)
      ambiguous  <- navigator.record(fixation)
    yield
      assertEquals(missing, Left(NavigationError.UnboundFixation(fixation)))
      assert(original.isRight)
      assertEquals(
        equivalent,
        original,
        "different recipes sharing the exact input/source/record resolve safely"
      )
      ambiguous match
        case Left(NavigationError.AmbiguousFixation(ref, contexts)) =>
          assertEquals(ref, fixation)
          assertEquals(contexts.map(_.source).distinct.size, 2)
          assertEquals(contexts.map(_.record).distinct.size, 1)
        case other => fail(s"expected explicit ambiguity, got $other")
  }
