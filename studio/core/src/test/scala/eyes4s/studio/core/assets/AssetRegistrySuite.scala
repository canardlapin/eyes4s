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

package eyes4s.studio.core.assets

import eyes4s.codec.ByteDigest
import eyes4s.studio.core.backend.{DatasetRevision, Phase, TrialKey}
import eyes4s.studio.core.document.*
import eyes4s.studio.core.fixture.{GoldenAssets, StoryMoments}
import io.circe.Json
import io.circe.syntax.*
import org.scalacheck.{Gen, Prop}

import java.nio.charset.StandardCharsets.UTF_8

/** The asset registry (ticket S2.10): display kinds, assets by byte digest,
  * placement, and the golden fixture's 257 of 259 images.
  */
class AssetRegistrySuite extends munit.ScalaCheckSuite:

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100)

  private def ok[E, A](e: Either[E, A]): A = e.fold(m => fail(s"unexpected: $m"), identity)

  private lazy val r3: DatasetRevisionSpec =
    ok(StoryMoments.t2).dataset(StoryMoments.r3).getOrElse(fail("t2 has no r3"))
  private lazy val golden: AssetRegistry = ok(GoldenAssets.registry(r3))

  private def key(p: String, t: String): TrialKey =
    TrialKey(p, if t.startsWith("enc") then Phase.Encoding else Phase.Retrieval, t, 1)
  private def file(name: String): AssetFile              = ok(AssetFile.of(name))
  private def item(k: TrialKey, name: String): MatchItem = ok(MatchItem.of(k, name))
  private def digestOf(name: String): ByteDigest         =
    ByteDigest.sha256(IArray.from(name.getBytes(UTF_8)))
  private def ref(name: String): AssetRef = AssetRef(file(name), digestOf(name))
  private val frame: ImagePlacement       = ok(ImagePlacement.of(448, 156, 1024, 768))
  private val screen: ScreenSize          = ok(ScreenSize.of(1920, 1080))
  private val inventory: ByteDigest       = digestOf("trials.csv")

  // -------------------------------------------------------------------------
  // The golden fixture
  // -------------------------------------------------------------------------

  test(
    "golden: 960 trials, 480 images and 480 blank + fixation cross, all on the image frame"
  ) {
    assertEquals(golden.trials.size, 960)
    assertEquals(golden.count(DisplayKind.Image), 480)
    assertEquals(golden.count(DisplayKind.BlankWithFixationCross), 480)
    assertEquals(golden.count(DisplayKind.Blank), 0)
    assert(golden.trials.forall(_.placement == r3.geometry.image))
    assertEquals(golden.screen, r3.geometry.screen)
    assertEquals((golden.dataset, golden.inventory), (r3.id, r3.sources.trials.get.bytes))
    assertEquals(AssetRegistry.check(golden, r3), Right(()))
    // Retrieval here = "Blank + fixation cross" (DESIGN_SPEC section 9).
    val retrieval = golden.trials.filter(_.trial.phase == Phase.Retrieval)
    assertEquals(retrieval.size, 480)
    assert(retrieval.forall(_.state == DisplayState.BlankWithFixationCross))
  }

  test("golden: 257 of 259 images present; forest-044 and kitchen-081 missing (P01, P24)") {
    val summary = golden.summary
    assertEquals((summary.files, summary.present), (259, 257))
    assertEquals(
      summary.missing.map(m =>
        (m.file.value, m.trials.map(t => s"${t.participant} ${t.trial}"))
      ),
      Vector(
        "forest-044.png"  -> Vector("P01 enc_08", "P24 enc_18"),
        "kitchen-081.png" -> Vector("P01 enc_15", "P24 enc_05")
      )
    )
    // Every stored stimulus is the asset the displays reference, by digest.
    assertEquals(golden.assets, ok(GoldenAssets.stimuli).map(_.asset))
  }

  test("missing is a state, never blank: every missing image renders as MissingAsset") {
    val missing = golden.trials.filter(_.isMissing)
    assertEquals(missing.size, 4)
    missing.foreach { d =>
      assertEquals(d.state, DisplayState.MissingAsset(DisplayKind.Image, d.asset.get.fileName))
      assertNotEquals(d.state, DisplayState.Blank)
      assertNotEquals(d.state, DisplayState.BlankWithFixationCross)
    }
    val states = golden.trials.map(_.state)
    assertEquals(states.count(_.isInstanceOf[DisplayState.Image]), 476)
    assertEquals(states.count(_.isInstanceOf[DisplayState.MissingAsset]), 4)
  }

  test("imagery trials sharing one blank display never collapse onto one match item") {
    val retrieval = golden.trials.filter(_.trial.phase == Phase.Retrieval)
    // Every retrieval trial shows the same display on the same frame ...
    assertEquals(retrieval.map(d => (d.display, d.placement)).distinct.size, 1)
    // ... yet each keeps its own item: 20 distinct items per participant, and
    // each is the trial's own attribute from the inventory.
    val rows = ok(GoldenAssets.rows).map(r => r.trial -> r.item).toMap
    retrieval.foreach(d =>
      assertEquals(golden.matchItem(d.trial).map(_.value), rows.get(d.trial))
    )
    val perParticipant =
      retrieval.groupBy(_.trial.participant).view.mapValues(_.flatMap(_.item))
    assertEquals(perParticipant.size, 24)
    perParticipant.foreach((p, items) => assertEquals((p, items.distinct.size), (p, 20)))
    assertEquals(golden.matchItem(key("P17", "ret_07")).map(_.value), Some("beach-042"))
    assertEquals(golden.matchItem(key("P17", "enc_03")).map(_.value), Some("beach-042"))
  }

  test("two trials with one blank display and different items keep both items") {
    val a        = key("P01", "ret_01")
    val b        = key("P01", "ret_02")
    val registry = ok(
      AssetRegistry.of(
        DatasetRevision(1),
        inventory,
        screen,
        Vector(
          TrialDisplay(a, Some(item(a, "beach-042")), Display.BlankWithFixationCross, frame),
          TrialDisplay(b, Some(item(b, "dog-229")), Display.BlankWithFixationCross, frame)
        )
      )
    )
    assertEquals(registry.matchItem(a).map(_.value), Some("beach-042"))
    assertEquals(registry.matchItem(b).map(_.value), Some("dog-229"))
    assertEquals(registry.trials.map(_.display).distinct.size, 1)
    assertEquals(registry.summary, AssetSummary(0, 0, Vector.empty))
  }

  test("repair resolves a missing file to a stored asset; a file not missing is refused") {
    val stored   = ref("forest-044-found.png")
    val repaired = ok(golden.repair(file("forest-044.png"), stored))
    assertEquals(repaired.summary.present, 258)
    assertEquals(repaired.missing.map(_.file.value), Vector("kitchen-081.png"))
    assertEquals(
      repaired.display(key("P01", "enc_08")).map(_.state),
      Some(DisplayState.Image(stored))
    )
    assertEquals(
      golden.repair(file("beach-042.png"), stored),
      Left(
        AssetError.NotMissing(
          file("beach-042.png"),
          Vector("forest-044", "kitchen-081").map(n => file(s"$n.png"))
        )
      )
    )
  }

  // -------------------------------------------------------------------------
  // Construction rules
  // -------------------------------------------------------------------------

  private val t1 = key("P01", "enc_01")

  test("an image must name an asset; a blank must not; cue and unknown may") {
    val link = Some(AssetLink.Missing(file("x.png")))
    assertEquals(
      Display.of(t1, DisplayKind.Image, None),
      Left(AssetError.AssetRequired(t1, DisplayKind.Image))
    )
    assertEquals(
      Display.of(t1, DisplayKind.Blank, link),
      Left(AssetError.AssetNotShown(t1, DisplayKind.Blank, file("x.png")))
    )
    assertEquals(
      Display.of(t1, DisplayKind.BlankWithFixationCross, link).left.map(_.message),
      Left(
        "Trial P01 · enc_01 shows blank+fixation-cross, which displays no asset, but names x.png."
      )
    )
    assertEquals(Display.of(t1, DisplayKind.Cue, None), Right(Display.Cue(None)))
    assertEquals(Display.of(t1, DisplayKind.Unknown, link), Right(Display.Unknown(link)))
    assertEquals(
      Display.of(t1, DisplayKind.Cue, link).map(_.state),
      Right(DisplayState.MissingAsset(DisplayKind.Cue, file("x.png")))
    )
  }

  test("inventory tokens: the five kinds parse, an empty cell is Unknown, others are refused") {
    assertEquals(
      DisplayKind.values.toVector.map(k => DisplayKind.parse(t1, k.token)),
      DisplayKind.values.toVector.map(Right(_))
    )
    assertEquals(DisplayKind.parse(t1, "  "), Right(DisplayKind.Unknown))
    assertEquals(
      DisplayKind.parse(t1, "Picture").left.map(_.message),
      Left(
        "Trial P01 · enc_01 has display kind 'Picture', which is not one of image, blank, " +
          "blank+fixation-cross, cue, unknown."
      )
    )
  }

  test("file names are one path segment") {
    assertEquals(
      AssetFile.of("a/b.png").left.map(_.message),
      Left("Asset file name 'a/b.png' is refused: it is not a single path segment.")
    )
    assert(AssetFile.of(" ").isLeft)
    assert(AssetFile.of("..").isLeft)
    assert(AssetFile.of("a\\b.png").isLeft)
  }

  test("the registry refuses duplicate trials, off-screen frames and inconsistent assets") {
    val t2        = key("P01", "enc_02")
    val offScreen = ok(ImagePlacement.of(1000, 500, 1024, 768))
    def image(k: TrialKey, link: AssetLink, p: ImagePlacement = frame) =
      TrialDisplay(k, None, Display.Image(link), p)
    def build(ds: TrialDisplay*) =
      AssetRegistry.of(DatasetRevision(1), inventory, screen, ds.toVector)
    val a = AssetLink.Present(ref("a.png"))
    assertEquals(build(image(t1, a), image(t1, a)), Left(AssetError.DuplicateTrial(t1)))
    assertEquals(
      build(image(t1, a, offScreen)).left.map(_.message),
      Left(
        "Trial P01 · enc_01's image frame 1024×768 px at (1000, 500) is not inside the " +
          "1920×1080 px screen."
      )
    )
    val other = AssetLink.Present(AssetRef(file("a.png"), digestOf("other")))
    assertEquals(
      build(image(t1, a), image(t2, other)),
      Left(
        AssetError.ConflictingDigests(
          file("a.png"),
          Vector(digestOf("a.png"), digestOf("other"))
        )
      )
    )
    assertEquals(
      build(image(t1, a), image(t2, AssetLink.Missing(file("a.png")))),
      Left(AssetError.PresentAndMissing(file("a.png"), t1, t2))
    )
  }

  test("fromRows: stored files are present, others missing; bad rows are refused by trial") {
    val rows = Vector(
      DisplayRow(t1, "beach-042", "image", "a.png"),
      DisplayRow(key("P01", "enc_02"), "dog-229", "image", "b.png"),
      DisplayRow(key("P01", "ret_01"), "beach-042", "blank+fixation-cross", ""),
      DisplayRow(key("P01", "ret_02"), "", "", "")
    )
    val registry = ok(AssetRegistry.fromRows(r3, rows, Vector(ref("a.png"))))
    assertEquals(
      registry.trials.map(_.state),
      Vector(
        DisplayState.Image(ref("a.png")),
        DisplayState.MissingAsset(DisplayKind.Image, file("b.png")),
        DisplayState.BlankWithFixationCross,
        DisplayState.Unknown(None)
      )
    )
    assertEquals(
      registry.trials.map(_.item.map(_.value)),
      Vector(Some("beach-042"), Some("dog-229"), Some("beach-042"), None)
    )
    assertEquals(
      AssetRegistry.fromRows(r3, Vector(DisplayRow(t1, "x", "image", "")), Vector.empty),
      Left(AssetError.AssetRequired(t1, DisplayKind.Image))
    )
    assertEquals(
      AssetRegistry.fromRows(r3, rows.take(1), Vector(ref("a.png"), ref("a.png"))),
      Left(AssetError.DuplicateAsset(file("a.png")))
    )
    val noInventory = r3.copy(
      id = DatasetRevision(9),
      sources = ok(Sources.of(r3.sources.entries.filter(_.role == SourceRole.Fixations)))
    )
    assertEquals(
      AssetRegistry.fromRows(noInventory, rows, Vector.empty),
      Left(AssetError.NoTrialInventory(DatasetRevision(9)))
    )
  }

  test("check ties a registry to its dataset revision and trial inventory bytes") {
    val t2 = ok(StoryMoments.t2)
    val r2 = t2.dataset(StoryMoments.r2).get
    assertEquals(
      AssetRegistry.check(golden, r2),
      Left(AssetError.WrongDataset(StoryMoments.r3, StoryMoments.r2))
    )
    val other = r3.copy(sources =
      ok(
        Sources.of(
          r3.sources.entries.map(s =>
            if s.role == SourceRole.Trials then s.copy(bytes = inventory) else s
          )
        )
      )
    )
    assertEquals(
      AssetRegistry.check(golden, other),
      Left(AssetError.WrongInventory(StoryMoments.r3, r3.sources.trials.get.bytes, inventory))
    )
  }

  // -------------------------------------------------------------------------
  // Codec
  // -------------------------------------------------------------------------

  private val sample: AssetRegistry =
    val enc = key("P01", "enc_08")
    val ret = key("P01", "ret_02")
    ok(
      AssetRegistry.of(
        DatasetRevision(3),
        inventory,
        screen,
        Vector(
          TrialDisplay(
            t1,
            Some(item(t1, "beach-007")),
            Display.Image(AssetLink.Present(ref("beach-007.png"))),
            frame
          ),
          TrialDisplay(
            enc,
            Some(item(enc, "forest-044")),
            Display.Image(AssetLink.Missing(file("forest-044.png"))),
            frame
          ),
          TrialDisplay(
            ret,
            Some(item(ret, "forest-044")),
            Display.BlankWithFixationCross,
            frame
          ),
          TrialDisplay(key("P01", "ret_03"), None, Display.Cue(None), frame)
        )
      )
    )

  test("the codec's wire form is pinned") {
    assertEquals(ok(AssetRegistry.encode(sample)).noSpaces, AssetRegistrySuite.samplePin)
    assertEquals(AssetRegistry.codec.map(_.schema.name), Right("studio.asset-registry"))
  }

  test("codec round trip: the sample and the golden registry") {
    assertEquals(AssetRegistry.decode(ok(AssetRegistry.encode(sample))), Right(sample))
    val json = ok(AssetRegistry.encode(golden))
    assertEquals(AssetRegistry.decode(json), Right(golden))
    assertEquals(
      AssetRegistry.digest(golden).map(_.sha256.hex),
      Right(AssetRegistrySuite.goldenDigest)
    )
  }

  test("a missing asset never decodes as a blank") {
    val json    = ok(AssetRegistry.encode(sample))
    val missing =
      json.hcursor.downField("value").downField("trials").downN(1).downField("display")
    assertEquals(
      missing.focus.map(_.noSpaces),
      Some("""{"Image":{"asset":{"Missing":{"file":"forest-044.png"}}}}""")
    )
    // Dropping the asset, or relabelling the display as a blank, is refused.
    def swap(display: Json): Either[?, AssetRegistry] =
      AssetRegistry.decode(missing.set(display).top.get)
    assert(swap(Json.obj("Image" -> Json.obj())).isLeft)
    assert(swap(Json.obj("Image" -> Json.obj("asset" -> Json.Null))).isLeft)
    assert(
      swap(
        Json.obj(
          "Blank" -> Json.obj(
            "asset" -> (AssetLink.Missing(file("forest-044.png")): AssetLink).asJson
          )
        )
      ).isLeft
    )
    assert(
      swap(Json.obj("Image" -> Json.obj("asset" -> Json.obj("Missing" -> Json.obj())))).isLeft
    )
  }

  // Generated registries: every kind, present and missing links, any frame
  // inside the screen, with or without items.
  private val files = Vector("a.png", "b.png", "c.png", "d.png")

  private val registries: Gen[AssetRegistry] =
    for
      present  <- Gen.someOf(files).map(_.toSet)
      n        <- Gen.choose(0, 12)
      displays <- Gen.listOfN(
        n,
        for
          name <- Gen.oneOf(files)
          link =
            if present(name) then AssetLink.Present(ref(name))
            else AssetLink.Missing(file(name))
          maybe <- Gen.option(Gen.const(link))
          d     <- Gen.oneOf(
            Display.Image(link),
            Display.Blank,
            Display.BlankWithFixationCross,
            Display.Cue(maybe),
            Display.Unknown(maybe)
          )
          name <- Gen.option(Gen.oneOf("beach-042", "dog-229", "forest-044"))
          w    <- Gen.choose(1, 1024)
          h    <- Gen.choose(1, 768)
          l    <- Gen.choose(0, 1920 - w)
          t    <- Gen.choose(0, 1080 - h)
        yield (d, name, ok(ImagePlacement.of(l, t, w, h)))
      )
    yield ok(
      AssetRegistry.of(
        DatasetRevision(2),
        inventory,
        screen,
        displays.toVector.zipWithIndex.map { case ((d, name, p), i) =>
          val k = key(s"P${i % 3}", s"ret_$i")
          TrialDisplay(k, name.map(item(k, _)), d, p)
        }
      )
    )

  property("every generated registry round-trips, and missing links stay missing") {
    Prop.forAll(registries) { r =>
      val back = AssetRegistry.decode(ok(AssetRegistry.encode(r)))
      assertEquals(back, Right(r))
      assertEquals(back.map(_.trials.map(_.state)), Right(r.trials.map(_.state)))
      assertEquals(back.map(_.missing), Right(r.missing))
    }
  }

  /** The pinned wire form of the sample registry, schema version 1, and the
    * CR3 digest of the golden fixture's registry (dataset r3 of the stories).
    */

  // -------------------------------------------------------------------------
  // From the project's own stored trial inventory (S5.7)
  // -------------------------------------------------------------------------

  /** A small trials.csv: two encoding images (one missing) and two imagery
    * trials that share one blank display.
    */
  private val smallCsv: String =
    """participant,phase,trial,occurrence,item,display_kind,image_file,response
      |P01,Encoding,enc_01,1,beach-042,image,beach-042.png,
      |P01,Encoding,enc_02,1,forest-044,image,forest-044.png,
      |P01,Retrieval,ret_01,1,beach-042,blank+fixation-cross,,remembered
      |P01,Retrieval,ret_02,1,forest-044,blank+fixation-cross,,forgotten
      |""".stripMargin

  private def bytes(text: String): IArray[Byte] = IArray.from(text.getBytes(UTF_8))

  private val displayColumns =
    DisplayColumns(ok(ColumnName.of("display_kind")), Some(ok(ColumnName.of("image_file"))))

  /** r3 storing `text` as its trial inventory, reading `displays`. */
  private def storing(text: String, displays: Option[DisplayColumns]): DatasetRevisionSpec =
    val sources = ok(
      Sources.of(r3.sources.entries.map { s =>
        if s.role == SourceRole.Trials then s.copy(bytes = ByteDigest.sha256(bytes(text)))
        else s
      })
    )
    val mapping = r3.inventory.map(m => ok(InventoryMapping.withDisplays(m, displays)))
    r3.copy(sources = sources, inventory = mapping)

  test("fromInventory reads the stored inventory through its display columns") {
    val spec     = storing(smallCsv, Some(displayColumns))
    val registry = ok(
      AssetRegistry.fromInventory(
        spec,
        bytes(smallCsv),
        Vector(ref("beach-042.png")),
        Vector.empty
      )
    )
    assertEquals(
      registry.trials.map(t => (t.trial.trial, t.kind)),
      Vector(
        ("enc_01", DisplayKind.Image),
        ("enc_02", DisplayKind.Image),
        ("ret_01", DisplayKind.BlankWithFixationCross),
        ("ret_02", DisplayKind.BlankWithFixationCross)
      )
    )
    assertEquals(
      registry.summary,
      AssetSummary(
        2,
        1,
        Vector(MissingAsset(file("forest-044.png"), Vector(key("P01", "enc_02"))))
      )
    )
    // The two imagery trials share one blank display and keep their own items.
    assertEquals(
      Vector("ret_01", "ret_02").map(t => registry.matchItem(key("P01", t)).map(_.value)),
      Vector(Some("beach-042"), Some("forest-044"))
    )
  }

  test("fromInventory refuses other bytes, no display columns, and a table it cannot read") {
    val spec   = storing(smallCsv, Some(displayColumns))
    val edited = smallCsv.replace("beach-042.png", "beach-043.png")
    assertEquals(
      AssetRegistry.fromInventory(spec, bytes(edited), Vector.empty, Vector.empty),
      Left(
        AssetError.InventoryBytes(
          r3.id,
          ByteDigest.sha256(bytes(edited)),
          ByteDigest.sha256(bytes(smallCsv))
        )
      )
    )
    assertEquals(
      AssetRegistry
        .fromInventory(storing(smallCsv, None), bytes(smallCsv), Vector.empty, Vector.empty),
      Left(AssetError.NoDisplayColumns(r3.id))
    )
    val noFile = smallCsv.replace("image_file", "picture")
    assertEquals(
      AssetRegistry.fromInventory(
        storing(noFile, Some(displayColumns)),
        bytes(noFile),
        Vector.empty,
        Vector.empty
      ),
      Left(AssetError.InventoryColumnMissing(r3.id, "image_file"))
    )
    val ragged = smallCsv + "P01,Retrieval,ret_03,1\n"
    assertEquals(
      AssetRegistry.fromInventory(
        storing(ragged, Some(displayColumns)),
        bytes(ragged),
        Vector.empty,
        Vector.empty
      ),
      Left(AssetError.InventoryRagged(r3.id, 5, 4, 8))
    )
    val badOcc = smallCsv.replace("P01,Encoding,enc_01,1,", "P01,Encoding,enc_01,one,")
    assertEquals(
      AssetRegistry.fromInventory(
        storing(badOcc, Some(displayColumns)),
        bytes(badOcc),
        Vector.empty,
        Vector.empty
      ),
      Left(AssetError.BadOccurrence(r3.id, 1, "one"))
    )
  }

  test("a repair stands for the missing file under the inventory's name, with the new bytes") {
    val spec     = storing(smallCsv, Some(displayColumns))
    val fixed    = ref("forest_044_restored.png")
    val relink   = AssetRelink(r3.id, file("forest-044.png"), fixed)
    val other    = AssetRelink(DatasetRevision(2), file("beach-042.png"), ref("elsewhere.png"))
    val registry = ok(
      AssetRegistry.fromInventory(
        spec,
        bytes(smallCsv),
        Vector(ref("beach-042.png")),
        Vector(relink, other)
      )
    )
    assertEquals(registry.missing, Vector.empty)
    assertEquals(
      registry.display(key("P01", "enc_02")).map(_.state),
      Some(DisplayState.Image(AssetRef(file("forest-044.png"), fixed.sha256)))
    )
    // Another revision's repair does not apply here.
    assertEquals(
      registry.display(key("P01", "enc_01")).map(_.state),
      Some(DisplayState.Image(ref("beach-042.png")))
    )
  }

  test("a repair with the original file, stored under its own name, survives a fresh read") {
    val spec     = storing(smallCsv, Some(displayColumns))
    val original = ref("forest-044.png")
    val relink   = AssetRelink(r3.id, file("forest-044.png"), original)
    // The repair stored forest-044.png itself: a fresh read finds it present.
    val read = ok(
      AssetRegistry.fromInventory(
        spec,
        bytes(smallCsv),
        Vector(ref("beach-042.png"), original),
        Vector.empty
      )
    )
    assertEquals(read.missing, Vector.empty)
    // Applying the repair again changes nothing.
    assertEquals(read.withRelinks(Vector(relink)), Right(read))
    // A repair with other bytes under the same name shows the repaired bytes.
    val other = AssetRef(file("forest-044.png"), digestOf("other bytes"))
    assertEquals(
      ok(read.withRelinks(Vector(AssetRelink(r3.id, file("forest-044.png"), other))))
        .display(key("P01", "enc_02"))
        .map(_.state),
      Some(DisplayState.Image(other))
    )
    // A repair of a file no display names is refused, naming it.
    assertEquals(
      read.withRelinks(Vector(AssetRelink(r3.id, file("nowhere.png"), original))),
      Left(AssetError.NotNamed(r3.id, file("nowhere.png")))
    )
  }

object AssetRegistrySuite:
  val samplePin: String =
    """{"schema":{"name":"studio.asset-registry","version":1},"value":{"dataset":3,"inventory":"d8ebe88dc3b6f5230da160d2057681ff7f6d89a2a474c7b4b978a20c68f79189","screen":{"height":1080,"width":1920},"trials":[{"display":{"Image":{"asset":{"Present":{"file":"beach-007.png","sha256":"0781aa307cd21488b92534f6268d9b871e01917bd0324c1a20aa8860fb1d6b4a"}}}},"item":"beach-007","placement":{"height":768,"left":448,"top":156,"width":1024},"trial":{"occurrence":1,"participant":"P01","phase":"Encoding","trial":"enc_01"}},{"display":{"Image":{"asset":{"Missing":{"file":"forest-044.png"}}}},"item":"forest-044","placement":{"height":768,"left":448,"top":156,"width":1024},"trial":{"occurrence":1,"participant":"P01","phase":"Encoding","trial":"enc_08"}},{"display":{"BlankWithFixationCross":{}},"item":"forest-044","placement":{"height":768,"left":448,"top":156,"width":1024},"trial":{"occurrence":1,"participant":"P01","phase":"Retrieval","trial":"ret_02"}},{"display":{"Cue":{"asset":null}},"item":null,"placement":{"height":768,"left":448,"top":156,"width":1024},"trial":{"occurrence":1,"participant":"P01","phase":"Retrieval","trial":"ret_03"}}]}}"""

  val goldenDigest: String = "9e603c2d02ba1a9e6558c5553555405155858bbcf8a8aa6da26755029037f70b"
