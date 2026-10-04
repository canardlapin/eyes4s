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
  * for the log, and is copied aside (stamped) before a save replaces it; an
  * unreadable file is never replaced; the recent-projects list stays
  * normalised, distinct, most recent first and capped.
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
    InMemoryPlatform
      .create[IO]()
      .map(p => (p, PreferencesStore(p.platform.files, file, IO.pure(1790000000000L))))

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
      assertEquals(saved, Right(None))
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

  private val stampOf                     = 1790000000000L
  private def copyAt(stamp: Long)         = path(s"${file.value}.corrupt-$stamp")
  private def textOf(bytes: IArray[Byte]) =
    String(IArray.genericWrapArray(bytes).toArray, UTF_8)

  /** Load a file holding `bytes`, then save over it: what was read, the
    * save's answer, the kept copy and the file afterwards.
    */
  private def loadedThenSaved(bytes: Array[Byte]) =
    for
      (p, s) <- store
      _      <- p.platform.files.write(file, IArray.unsafeFromArray(bytes)).map(right)
      result <- s.load
      before <- p.platform.files.read(copyAt(stampOf))
      saved  <- s.save(UserPreferences.defaults.withAppearance(AppearanceChoice.Dark))
      kept   <- p.platform.files.read(copyAt(stampOf))
      reload <- s.load
    yield (result, before.isLeft, saved, kept.map(textOf), reload)

  private val corrupt: Vector[(String, String)] = Vector(
    "garbage"         -> "\u0000\u0001 not json {",
    "truncated"       -> UserPreferences.encode(UserPreferences.defaults).dropRight(7),
    "wrong shape"     -> """{"version": 1, "preferences": {"appearance": 3}}""",
    "no version"      -> """{"preferences": {}}""",
    "a newer version" -> """{"version": 2, "preferences": {"anything": true}}""",
    "a blank path"    ->
      """{"version": 1, "preferences": {"appearance": {"Dark": {}}, "stage": {"Dark": {}},
        |"recentProjects": ["  "], "layouts": [], "exportDirectory": null}}""".stripMargin,
    "two layouts of a perspective" ->
      """{"version": 1, "preferences": {"appearance": {"Dark": {}}, "stage": {"Dark": {}},
        |"recentProjects": [], "layouts": [{"perspective": {"Data": {}}, "layout": "a"},
        |{"perspective": {"Data": {}}, "layout": "b"}], "exportDirectory": null}}""".stripMargin
  )

  corrupt.foreach { (name, text) =>
    test(s"a corrupt file ($name): defaults and a problem; it is copied aside before a save") {
      loadedThenSaved(text.getBytes(UTF_8)).map { (result, notYetCopied, saved, kept, reload) =>
        val (prefs, problems) = result
        assertEquals(prefs, UserPreferences.defaults)
        problems match
          case Vector(PreferencesProblem.Corrupt(`file`, _)) => ()
          case other => fail(s"expected one Corrupt problem, got $other")
        assert(problems.head.message.contains("using defaults"), problems.head.message)
        // Reading touches nothing; the save keeps the file before replacing it.
        assert(notYetCopied)
        assertEquals(saved, Right(Some(copyAt(stampOf))))
        assertEquals(kept, Right(text))
        assertEquals(
          reload,
          (UserPreferences.defaults.withAppearance(AppearanceChoice.Dark), Vector.empty)
        )
      }
    }
  }

  test("a newer schema version is not read as this one") {
    loadedThenSaved("""{"version": 2, "preferences": {"anything": true}}""".getBytes(UTF_8))
      .map { (result, _, _, _, _) =>
        assertEquals(
          result._2.collect { case PreferencesProblem.Corrupt(_, e) => e },
          Vector(PreferencesError.UnknownVersion(2, 1))
        )
      }
  }

  test("an unreadable file is the defaults and a problem, and a save does not replace it") {
    for
      (p, s) <- store
      _      <- p.makeDirectory(file)
      result <- s.load
      saved  <- s.save(UserPreferences.defaults.withAppearance(AppearanceChoice.Dark))
    yield
      assertEquals(result._1, UserPreferences.defaults)
      result._2 match
        case Vector(PreferencesProblem.Unreadable(`file`, _: PlatformError)) => ()
        case other => fail(s"expected one Unreadable problem, got $other")
      saved match
        case Left(pr @ PreferencesProblem.NotOverwritten(`file`, _)) =>
          assert(pr.message.contains("kept as it is"), pr.message)
        case other => fail(s"expected NotOverwritten, got $other")
  }

  test("successive copies never replace one another: each is stamped with its save") {
    for
      p     <- InMemoryPlatform.create[IO]()
      clock <- cats.effect.Ref[IO].of(stampOf)
      s = PreferencesStore(p.platform.files, file, clock.getAndUpdate(_ + 1))
      _  <- p.platform.files.write(file, IArray.unsafeFromArray("{".getBytes(UTF_8))).map(right)
      a  <- s.save(UserPreferences.defaults)
      _  <- p.platform.files.write(file, IArray.unsafeFromArray("[".getBytes(UTF_8))).map(right)
      b  <- s.save(UserPreferences.defaults)
      ka <- p.platform.files.read(copyAt(stampOf))
      kb <- p.platform.files.read(copyAt(stampOf + 1))
    yield
      assertEquals((a, b), (Right(Some(copyAt(stampOf))), Right(Some(copyAt(stampOf + 1)))))
      assertEquals((ka.map(textOf), kb.map(textOf)), (Right("{"), Right("[")))
  }

  test("a usable file is replaced with no copy") {
    for
      (p, s) <- store
      _      <- s.save(UserPreferences.defaults).map(right)
      again  <- s.save(UserPreferences.defaults.withAppearance(AppearanceChoice.System))
      copy   <- p.platform.files.read(copyAt(stampOf))
    yield
      assertEquals(again, Right(None))
      assert(copy.isLeft)
  }

  // --- the fields -----------------------------------------------------------------------------

  test("recent paths are normalised before they are compared: a trailing separator") {
    val a = path("/p/study.eyes")
    val r = RecentProjects.empty.opened(a).opened(path("/p/study.eyes/"))
    assertEquals(r.paths, Vector(a))
    assertEquals(r.forget(path("/p/study.eyes//")).paths, Vector.empty)
    assertEquals(RecentProjects.normalise(path("/")), path("/"))
  }

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
