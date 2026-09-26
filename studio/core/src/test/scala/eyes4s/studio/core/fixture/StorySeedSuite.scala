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

package eyes4s.studio.core.fixture

import cats.effect.IO
import cats.syntax.all.*
import eyes4s.codec.ByteDigest
import eyes4s.studio.core.assets.DisplayKind
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.bundle.*
import eyes4s.studio.core.document.{AdmissionDecision, Perspective, SourceRole, StudioDocument}
import eyes4s.studio.core.freshness.*
import munit.CatsEffectSuite

/** The story seeds (ticket S0.8): each reproduces its board's state facts,
  * the fake backend agrees with its document, and S2.7's freshness on it is
  * what the boards show. The bundled example opens as a copy.
  */
class StorySeedSuite extends CatsEffectSuite:
  import StoryMoments.{r2, r3, rev3, rev4, rev5, run5, run6, run7, run8, run8Job}

  private def ok[E, A](e: Either[E, A]): A = e.fold(m => fail(s"unexpected: $m"), identity)

  private def seed(moment: StoryMoment): IO[StorySeed[IO]] =
    StorySeed.load[IO](moment).map(_.fold(e => fail(e.message), identity))

  /** The fake's run state and the derived standing say the same thing. */
  private def agrees(state: RunState, standing: RunStanding): Boolean = (state, standing) match
    case (RunState.Current, RunStanding.Current)           => true
    case (RunState.Stale, RunStanding.Stale(_))            => true
    case (RunState.Running(_), RunStanding.Running)        => true
    case (RunState.Cancelled(a), RunStanding.Cancelled(b)) => a == b
    case (RunState.Failed, RunStanding.Failed)             => true
    case _                                                 => false

  private def backendAgrees(s: StorySeed[IO]): IO[Unit] =
    s.backend.runs.map { runs =>
      val f = s.freshness
      runs.foreach { r =>
        val standing = f.standing(r.run).getOrElse(fail(s"no standing for run ${r.run.number}"))
        assert(
          agrees(r.state, standing),
          s"run ${r.run.number}: fake ${r.state}, derived $standing"
        )
      }
      assertEquals(runs.map(_.run), s.document.runs.map(_.id))
    }

  private def figures(f: Freshness): Vector[String] = f.figures.map(FreshnessText.figure)

  private def assetsOf(s: StorySeed[IO]): Unit =
    assertEquals(s.assets.dataset, r3)
    assertEquals((s.assets.summary.files, s.assets.summary.present), (259, 257))
    assertEquals(s.assets.count(DisplayKind.BlankWithFixationCross), 480)

  test("t1 Data · verify: r3 pending, run 5 on r2 current and would go stale, no jobs") {
    seed(StoryMoment.T1).flatMap { s =>
      val d = s.document
      assertEquals(d.dataset(r3).map(_.decision), Some(AdmissionDecision.Pending))
      assertEquals(d.runs.map(r => (r.id, r.analysis, r.dataset)), Vector((run5, rev3, r2)))
      assertEquals(d.runsOn(r3), Vector.empty)
      assertEquals(d.presentation.perspective, Perspective.Data)
      assertEquals(s.session, SessionFacts.empty)
      val f = s.freshness
      assertEquals(FreshnessText.badge(f.badge), "Analysis rev 3 · run 5 · data r2 · current")
      assertEquals(f.draft, None)
      assertEquals(FreshnessText.jobs(f.activity), "No jobs")
      assertEquals(
        f.pending.flatMap(FreshnessText.pending(_, d.runs)),
        Vector("Run 5 (rev 3) used r2 · becomes stale when r3 is admitted")
      )
      assertEquals(figures(f), Vector("Figure 2 · run 5 · rev 3 · data r2 · current"))
      assetsOf(s)
      backendAgrees(s) >>
        s.backend.admission(r3).map(a => assert(a.isRight, a))
    }
  }

  test("t2: rev 4 · run 7 · r3 current; draft rev 5 checked ready on the fake; no jobs") {
    seed(StoryMoment.T2).flatMap { s =>
      val d = s.document
      assertEquals(
        d.runs.map(r => (r.id, r.analysis, r.dataset)),
        Vector((run5, rev3, r2), (run6, rev4, r3), (run7, rev4, r3))
      )
      assertEquals(d.draft.map(x => (x.id, x.base, x.changeCount)), Some((rev5, rev4, 1)))
      assertEquals(s.session.draftCheck, DraftCheck.Checked(d.draft.get, Vector.empty))
      assertEquals(s.session.progress, Vector.empty)
      val f = s.freshness
      assertEquals(FreshnessText.badge(f.badge), "Analysis rev 4 · run 7 · data r3 · current")
      assertEquals(f.draft.map(FreshnessText.draft), Some("Draft rev 5 · 1 change · ready"))
      assertEquals(FreshnessText.jobs(f.activity), "No jobs")
      assertEquals(
        f.bannerFor(Perspective.Compare).map(FreshnessText.banner),
        Some("Showing run 7 (analysis rev 4). Draft rev 5 adds σ 8° and has not been run.")
      )
      assertEquals(f.bannerFor(Perspective.Analysis), None)
      assertEquals(
        figures(f),
        Vector(
          "Figure 1 · run 7 · rev 4 · data r3 · current",
          "Figure 2 · run 5 · rev 3 · data r2 · stale"
        )
      )
      assetsOf(s)
      backendAgrees(s) >> s.backend.jobs.map(j => assertEquals(j, Vector.empty))
    }
  }

  test("t3: run 8 held mid-run on the fake at 21,400 / 44,845 pairs; the view stays on run 7") {
    seed(StoryMoment.T3).flatMap { s =>
      val d = s.document
      assertEquals(d.draft, None)
      assertEquals(d.job(run8), Some(run8Job))
      assertEquals(d.presentation.shownRun, Some(run7))
      assertEquals(s.session.progress.map(p => (p.job, p.run)), Vector((run8Job, run8)))
      assertEquals(s.session.progress.map(_.totals.completedPairs), Vector(21400L))
      val f = s.freshness
      assertEquals(FreshnessText.badge(f.badge), "Showing analysis rev 4 · run 7 · data r3")
      assertEquals(FreshnessText.newer(f.badge), Some("Rev 5 · run 8 running · 48%"))
      assertEquals(FreshnessText.jobs(f.activity), "Run 8 · Comparing · 21,400 / 44,845 pairs")
      assertEquals(
        f.bannerFor(Perspective.Compare).map(FreshnessText.banner),
        Some(
          "Run 8 (rev 5, adds σ 8°) is running — results will not replace this view until " +
            "you choose Show."
        )
      )
      assertEquals(f.standing(run7), Some(RunStanding.Current))
      assertEquals(
        figures(f),
        Vector(
          "Figure 1 · run 7 · rev 4 · data r3 · current",
          "Figure 2 · run 5 · rev 3 · data r2 · stale"
        )
      )
      assetsOf(s)
      backendAgrees(s) >> s.backend.job(run8Job).map { j =>
        assert(j.exists(_.state.isInstanceOf[JobState.Running]), j)
      }
    }
  }

  test("each seed's document is a bundle; t3's job handle stays with the session") {
    StoryMoment.values.toVector.traverse(m => seed(m)).map { seeds =>
      seeds.foreach { s =>
        val inputs  = BundleSamples.inputsFor(s.document)
        val encoded = ok(ProjectBundle.encode(s.document, SharingOptions.complete, inputs))
        val parts   = encoded.parts.toMap
        val back    = ok(
          ProjectBundle.assemble(
            encoded.manifest,
            p => parts.get(p).toRight(BundleError.Store(StoreError.Missing(p)))
          )
        )
        assertEquals(back.document, ok(s.document.withJobs(Vector.empty)))
        assert(back.science.verified)
        assertEquals(encoded.sessionOnly, s.document.jobs)
      }
      assertEquals(seeds.map(_.document.jobs.size), Vector(0, 0, 1))
    }
  }

  // -------------------------------------------------------------------------
  // The bundled example (portable half; studio-desktop checks the files)
  // -------------------------------------------------------------------------

  private val me = ok(LockOwner.of("StorySeedSuite"))

  private def bytes(text: String): IArray[Byte] = BundleSamples.utf8(text)

  private val fixations = ExampleFile("fixations.csv", bytes("participant,x\nP01,1\n"))
  private val trials    = ExampleFile("trials.csv", bytes("participant,item\nP01,beach-007\n"))
  private val stimuli   =
    Vector("dog-229.png", "beach-007.png").map(n => ExampleFile(n, bytes(s"png:$n")))
  private val document: StudioDocument =
    BundleSamples.withSources(ok(ExampleProject.document), fixations.bytes, trials.bytes)

  private def written: IO[ProjectStore[IO]] =
    for
      store <- InMemoryProjectStore.create[IO]
      lock  <- store.acquire(me).map(ok)
      _     <- ExampleProject
        .write(store, lock, document, ExampleSources(fixations, trials, stimuli))
        .map(ok)
      _ <- store.release(lock).map(ok)
    yield store

  test("the example is written deterministically, with its sources and stimuli stored") {
    for
      a      <- written
      b      <- written
      listA  <- a.list.map(ok)
      listB  <- b.list.map(ok)
      manA   <- a.readManifest.map(ok)
      manB   <- b.readManifest.map(ok)
      opened <- ProjectBundle.open(a).map(ok)
    yield
      assertEquals(listA, listB)
      assertEquals(ByteDigest.sha256(manA), ByteDigest.sha256(manB))
      assertEquals(opened.document, document)
      assertEquals(
        opened.manifest.inputs.map(e => (e.kind, e.name.getOrElse(""))).toSet,
        Set(
          InputKind.StimulusImage                -> "beach-007.png",
          InputKind.StimulusImage                -> "dog-229.png",
          InputKind.Source(SourceRole.Fixations) -> "fixations.csv",
          InputKind.Source(SourceRole.Trials)    -> "trials.csv"
        )
      )
      assert(opened.manifest.inputs.forall(_.stored))
      assertEquals(
        ExampleProject.storedStimuli(opened.manifest).map(_.map(_.file.value).sorted),
        Right(Vector("beach-007.png", "dog-229.png"))
      )
  }

  test("opening the example copies it as 'memory-study (copy)' and leaves it unchanged") {
    for
      example <- written
      before  <- example.readManifest.map(ok)
      entries <- example.list.map(ok)
      target  <- InMemoryProjectStore.create[IO]
      lock    <- target.acquire(me).map(ok)
      copy    <- ExampleProject.openCopy(example, target, lock).map(ok)
      status  <- ProjectBundle.checkInputs(target, copy.project.manifest)
      after   <- example.readManifest.map(ok)
      again   <- ExampleProject.openCopy(example, target, lock)
      mine    <- example.acquire(me)
    yield
      assertEquals(copy.name, "memory-study (copy)")
      assertEquals(ExampleProject.copyNameOf("lab-study"), "lab-study (copy)")
      assertEquals(copy.project.document, document)
      assert(copy.project.science.verified)
      assert(status.forall(_.isInstanceOf[InputStatus.Present]), status)
      assertEquals(status.size, 4)
      assertEquals(ByteDigest.sha256(after), ByteDigest.sha256(before))
      assertEquals(entries.size, 13 + 4)
      // A second copy into the same bundle is refused, and the example was
      // never locked by the copy.
      assert(again.left.exists(_.isInstanceOf[BundleError.TargetNotEmpty]), again)
      assert(mine.isRight, mine)
  }
