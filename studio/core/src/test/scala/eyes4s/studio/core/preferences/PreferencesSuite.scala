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

package eyes4s.studio.core.preferences

import cats.effect.IO
import eyes4s.studio.core.document.{LayoutBlob, Perspective, SavedLayout, StageAppearance}
import eyes4s.studio.core.platform.{HostPath, InMemoryPlatform, PlatformError}
import munit.CatsEffectSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

import java.nio.charset.StandardCharsets.UTF_8

/** User preferences (ticket S2.8), JVM and JS: the versioned file round-trips
  * through the store on a host file system; a missing file is the defaults;
  * a corrupt, malformed or newer-version file is the defaults plus a problem
  * for the log, and is kept beside itself so the next save does not destroy
  * it; the recent-projects list stays distinct, most recent first and
  * capped.
  */
class PreferencesSuite extends CatsEffectSuite with munit.ScalaCheckSuite:

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100)

  private def right[E, A](e: Either[E, A]): A =
    e.fold(x => fail(s"unexpected Left: $x"), identity)

  private def path(text: String): HostPath = right(HostPath.of(text))

  private val file = path("/Users/me/Library/Application Support/Eyes Studio/preferences.json")

  // --- generators -----------------------------------------------------------------------

  private val hostPath: Gen[HostPath] =
    Gen
      .nonEmptyListOf(Gen.oneOf(Gen.alphaNumChar, Gen.oneOf(' ', '-', '.', 'é', '·')))
      .map(cs => path("/p/" + cs.mkString))

  private val preferences: Gen[UserPreferences] =
    for
      a  <- Gen.oneOf(AppearanceChoice.values.toSeq)
      s  <- Gen.oneOf(StageAppearance.values.toSeq)
      r  <- Gen.listOf(hostPath).map(ps => RecentProjects.of(ps.toVector))
      ps <- Gen.someOf(Perspective.values.toSeq)
      ls <- Gen.sequence[Vector[SavedLayout], SavedLayout](
        ps.map(p => Gen.asciiPrintableStr.map(b => SavedLayout(p, LayoutBlob(b))))
      )
      e <- Gen.option(hostPath)
    yield right(UserPreferences.of(a, s, r, ls, e))

  // --- round trip ---------------------------------------------------------------------------

  property("the file's text round-trips") {
    forAll(preferences)(p =>
      assertEquals(UserPreferences.decode(UserPreferences.encode(p)), Right(p))
    )
  }

  private def store: IO[(InMemoryPlatform[IO], PreferencesStore[IO])] =
    InMemoryPlatform.create[IO]().map(p => (p, PreferencesStore(p.platform.files, file)))

  test("round trip through the store: saved preferences load as saved, with no problem") {
    val p = right(
      UserPreferences.of(
        AppearanceChoice.System,
        StageAppearance.Mid,
        RecentProjects.of(Vector(path("/a/memory-study.eyes"), path("/b/pilot.eyes"))),
        Vector(SavedLayout(Perspective.Compare, LayoutBlob("""{"root":1}"""))),
        Some(path("/Users/me/exports"))
      )
    )
    for
      (_, s)   <- store
      saved    <- s.save(p)
      reloaded <- s.load
    yield
      assertEquals(saved, Right(()))
      assertEquals(reloaded, (p, Vector.empty))
  }

  test("no file is the defaults, with no problem") {
    store.flatMap(_._2.load).map(r => assertEquals(r, (UserPreferences.defaults, Vector.empty)))
  }

  test("the file is versioned") {
    val text = UserPreferences.encode(UserPreferences.defaults)
    val json = right(io.circe.parser.parse(text))
    assertEquals(json.hcursor.get[Int]("version"), Right(1))
    assert(json.hcursor.downField("preferences").downField("appearance").succeeded, text)
  }

  // --- corrupt files ------------------------------------------------------------------------

  private def loaded(bytes: Array[Byte]) =
    for
      (p, s) <- store
      _      <- p.platform.files.write(file, IArray.unsafeFromArray(bytes)).map(right)
      result <- s.load
      kept   <- p.platform.files.read(path(file.value + ".corrupt"))
    yield (result, kept.map(k => String(IArray.genericWrapArray(k).toArray, UTF_8)))

  private val corrupt: Vector[(String, String)] = Vector(
    "garbage"      -> "\u0000\u0001 not json {",
    "truncated"    -> UserPreferences.encode(UserPreferences.defaults).dropRight(7),
    "wrong shape"  -> """{"version": 1, "preferences": {"appearance": 3}}""",
    "no version"   -> """{"preferences": {}}""",
    "a blank path" ->
      """{"version": 1, "preferences": {"appearance": {"Dark": {}}, "stage": {"Dark": {}},
        |"recentProjects": ["  "], "layouts": [], "exportDirectory": null}}""".stripMargin,
    "two layouts of a perspective" ->
      """{"version": 1, "preferences": {"appearance": {"Dark": {}}, "stage": {"Dark": {}},
        |"recentProjects": [], "layouts": [{"perspective": {"Data": {}}, "layout": "a"},
        |{"perspective": {"Data": {}}, "layout": "b"}], "exportDirectory": null}}""".stripMargin
  )

  corrupt.foreach { (name, text) =>
    test(s"a corrupt file ($name) is the defaults, a problem for the log, and is kept") {
      loaded(text.getBytes(UTF_8)).map { case ((prefs, problems), kept) =>
        assertEquals(prefs, UserPreferences.defaults)
        problems match
          case Vector(PreferencesProblem.Corrupt(`file`, _, Some(k))) =>
            assertEquals(k, path(file.value + ".corrupt"))
          case other => fail(s"expected one Corrupt problem, got $other")
        assertEquals(kept, Right(text))
        assert(problems.head.message.contains("using defaults"), problems.head.message)
      }
    }
  }

  test("a newer schema version is not read as this one") {
    val newer = """{"version": 2, "preferences": {"anything": true}}"""
    loaded(newer.getBytes(UTF_8)).map { case ((prefs, problems), _) =>
      assertEquals(prefs, UserPreferences.defaults)
      assertEquals(
        problems.collect { case PreferencesProblem.Corrupt(_, e, _) => e },
        Vector(PreferencesError.UnknownVersion(2, 1))
      )
    }
  }

  test("an unreadable file is the defaults and a problem; a directory is not preferences") {
    for
      (p, s) <- store
      _      <- p.makeDirectory(file)
      result <- s.load
    yield
      assertEquals(result._1, UserPreferences.defaults)
      result._2 match
        case Vector(PreferencesProblem.Unreadable(`file`, _: PlatformError)) => ()
        case other => fail(s"expected one Unreadable problem, got $other")
  }

  test("saving over a corrupt file writes good preferences; the corrupt copy stays") {
    for
      (p, s) <- store
      _ <- p.platform.files.write(file, IArray.unsafeFromArray("{".getBytes(UTF_8))).map(right)
      first  <- s.load
      _      <- s.save(first._1.withAppearance(AppearanceChoice.Dark)).map(right)
      second <- s.load
      kept   <- p.platform.files.read(path(file.value + ".corrupt"))
    yield
      assertEquals(
        second,
        (UserPreferences.defaults.withAppearance(AppearanceChoice.Dark), Vector.empty)
      )
      assertEquals(kept.map(k => String(IArray.genericWrapArray(k).toArray, UTF_8)), Right("{"))
  }

  // --- the fields -----------------------------------------------------------------------------

  test("recent projects: most recent first, once each, at most ten; forget drops one") {
    val ps     = (1 to 12).toVector.map(i => path(s"/p/$i.eyes"))
    val recent = ps.foldLeft(RecentProjects.empty)(_.opened(_))
    assertEquals(recent.paths, ps.reverse.take(10))
    val again = recent.opened(ps(5))
    assertEquals(again.paths.head, ps(5))
    assertEquals(again.paths.size, 10)
    assertEquals(again.paths.distinct, again.paths)
    assertEquals(again.forget(ps(5)).paths, again.paths.tail)
  }

  test("one user layout per perspective, kept in perspective order") {
    val compare = SavedLayout(Perspective.Compare, LayoutBlob("c"))
    val data    = SavedLayout(Perspective.Data, LayoutBlob("d"))
    val p       = UserPreferences.defaults.withLayout(compare).withLayout(data)
    assertEquals(p.layouts, Vector(data, compare))
    val replaced = p.withLayout(SavedLayout(Perspective.Compare, LayoutBlob("c2")))
    assertEquals(replaced.layout(Perspective.Compare).map(_.layout.text), Some("c2"))
    assertEquals(replaced.layouts.size, 2)
    assertEquals(
      UserPreferences.of(
        AppearanceChoice.Light,
        StageAppearance.Dark,
        RecentProjects.empty,
        Vector(data, data),
        None
      ),
      Left(PreferencesError.DuplicateLayouts(Vector(Perspective.Data)))
    )
  }

  test("defaults: light, a dark stage, nothing recent, no layouts, no export directory") {
    val d = UserPreferences.defaults
    assertEquals(
      (d.appearance, d.stage, d.recent, d.layouts, d.exportDirectory),
      (AppearanceChoice.Light, StageAppearance.Dark, RecentProjects.empty, Vector.empty, None)
    )
  }
