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

import eyes4s.compare.*
import eyes4s.core.Weight
import eyes4s.surface.EdgePolicy
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import io.circe.Json

class DensityArchiveSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(x => fail(s"$x"), identity)
  private val results                       = StudyResultCodecs.cosine[Px]
  private val archiveCodec                  = new DensityArchiveCodec(results)
  private val input                         = get(
    StudyInputCodecs.study[Px].input.parse(StudyInputFixtures.inputVersionOne)
  )
  private val plan   = get(StudyCodecs.cosine[Px].codec.parse(SavedStudyFixtures.versionOne))
  private val result = get(results.codec.parse(StudyResultFixtures.resultVersionOne))
  private val key    = result.scales.head.estimation.head._1
  private val mass   = get(result.scales.head.estimation.head._2)

  private def same(actual: StudyResult[StudyKey, Px, Similarity, SignedDifference]): Unit =
    assertEquals(get(results.codec.encode(actual)), get(results.codec.encode(result)))

  private def doc(
      storage: DensityStorage
  ): (DensityArchiveBundle[StudyKey, Px, Unit, Similarity, SignedDifference], Json) =
    val bundle = get(archiveCodec.encode(result, storage))
    bundle -> get(archiveCodec.codec.encode(bundle.archive))

  private def changeOutcome(document: Json, scale: Int = 0, index: Int = 0)(
      f: Json => Json
  ): Json =
    val payload      = get(document.hcursor.get[Json]("value"))
    val scales       = get(payload.hcursor.get[Vector[Json]]("scales"))
    val rows         = get(scales(scale).hcursor.get[Vector[Json]]("estimation"))
    val changed      = rows(index).mapObject(o => o.add("outcome", f(o("outcome").get)))
    val changedScale =
      scales(scale).mapObject(_.add("estimation", Json.arr(rows.updated(index, changed)*)))
    document.mapObject(
      _.add(
        "value",
        payload.mapObject(_.add("scales", Json.arr(scales.updated(scale, changedScale)*)))
      )
    )

  private def storage(document: Json)(f: Json => Json): Json =
    changeOutcome(document)(_.mapObject(o => o.add("storage", f(o("storage").get))))

  private def provider(
      bundle: DensityArchiveBundle[StudyKey, Px, Unit, Similarity, SignedDifference]
  ): PayloadRef => Option[VerifiedPayload] =
    ref => bundle.chunks.find(_._2.ref == ref).map(_._2)

  test("v1 remains readable and writes its exact canonical document") {
    val old = get(archiveCodec.codec.parse(StudyResultFixtures.resultVersionOne))
    assertEquals(get(archiveCodec.codec.encode(old)), get(results.codec.encode(result)))
    same(get(old.materialize()))
    assertEquals(get(old.densityReader().density(0, key)).cells.toVector, mass.values.toVector)
  }

  test(
    "inline, packed and recomputable archives materialize through the completed-result gates"
  ) {
    DensityStorage.values.foreach { mode =>
      val (bundle, document) = doc(mode)
      val opened             = get(archiveCodec.codec.decode(document))
      val recompute          = archiveCodec.recomputer(plan, input, opened)
      same(get(opened.materialize(provider(bundle), Some(recompute))))
      assertEquals(
        get(
          opened.densityReader(provider(bundle), Some(recompute)).density(0, key)
        ).cells.toVector,
        mass.values.toVector
      )
    }
  }

  test("recomputable is the default and opening or constructing a reader calls no providers") {
    val bundle = get(archiveCodec.encode(result))
    assertEquals(bundle.chunks.size, 0)
    val document = get(archiveCodec.codec.encode(bundle.archive))
    assertEquals(get(document.hcursor.downField("schema").get[Int]("version")), 2)
    val opened    = get(archiveCodec.codec.decode(document))
    var calls     = Vector.empty[(Int, StudyKey)]
    val recompute = archiveCodec.recomputer(plan, input, opened)
    val reader    = opened.densityReader(
      _ => fail("unexpected payload request"),
      Some((s, k) => {
        calls :+= s -> k
        recompute(s, k)
      })
    )
    assertEquals(calls, Vector.empty)
    assertEquals(get(reader.density(0, key)).cells.toVector, mass.values.toVector)
    assertEquals(calls, Vector(0 -> key))
  }

  test("packed calls its provider once for one selected row") {
    val (bundle, _) = doc(DensityStorage.Packed)
    var calls       = 0
    val reader      = bundle.archive.densityReader(ref => { calls += 1; provider(bundle)(ref) })
    assertEquals(calls, 0)
    assertEquals(get(reader.density(0, key)).cells.toVector, mass.values.toVector)
    assertEquals(calls, 1)
  }

  test("packed chunks group all scales by participant and are float64 row-major") {
    val p = StudyResultFixtures.cosinePlan(
      StudyResultFixtures.clean,
      FailurePolicy.RequireAll,
      StudyResultFixtures.scales.take(2)
    )
    val r      = get(p.run(StudyResultFixtures.clean))
    val bundle = get(archiveCodec.encode(r, DensityStorage.Packed))
    assertEquals(bundle.chunks.map(_._1), Vector("s1", "s2"))
    bundle.chunks.foreach { (participant, chunk) =>
      val count = r.scales.flatMap(_.estimation).count { (key, outcome) =>
        key.participant == participant && outcome.isRight
      }
      assertEquals(chunk.ref.layout.shape, Vector(count, 2, 2))
      assertEquals(chunk.ref.layout.element, ElementKind.Float64)
      assertEquals(chunk.ref.layout.order, ArrayOrder.RowMajor)
    }
    assertEquals(
      get(results.codec.encode(get(bundle.archive.materialize(provider(bundle))))),
      get(results.codec.encode(r))
    )
  }

  test("map digest has pinned identical JVM and JS bytes") {
    assertEquals(
      get(DensityDigest.of(mass, result.description)).hex,
      "7dcf9634181353e3946f29af0cc8da83a35d7c6784c93fbb0fc95759762aaac0"
    )
  }

  test("same map has the same digest independent of storage") {
    val expected = get(DensityDigest.of(mass, result.description))
    DensityStorage.values.foreach { mode =>
      val (bundle, _) = doc(mode)
      val view        = get(
        bundle.archive
          .densityReader(
            provider(bundle),
            Some(archiveCodec.recomputer(plan, input, bundle.archive))
          )
          .density(0, key)
      )
      val restored = get(Surface.mass(view.geometry.grid, view.cells, view.provenance))
      assertEquals(get(DensityDigest.of(restored, result.description)), expected)
    }
  }

  test("a swapped packed row is refused with scale, key and expected versus actual digest") {
    val (bundle, document) = doc(DensityStorage.Packed)
    val forged             = storage(document)(_.mapObject(_.add("row", Json.fromInt(2))))
    val error              = get(archiveCodec.codec.decode(forged))
      .densityReader(provider(bundle))
      .density(0, key)
      .swap
    get(error) match
      case DensityError.DigestMismatch(s, k, expected, actual) =>
        assertEquals(s, 0); assertEquals(k, key); assert(expected != actual)
      case e => fail(e.message)
  }

  test("packed row bounds and float32 substitutions fail at opening") {
    val (_, document) = doc(DensityStorage.Packed)
    Vector(-1, 999).foreach { row =>
      assert(
        archiveCodec.codec
          .decode(storage(document)(_.mapObject(_.add("row", Json.fromInt(row)))))
          .isLeft
      )
    }
    val forged = storage(document)(
      _.mapObject(o =>
        o.add(
          "chunk",
          o("chunk").get.mapObject(c =>
            c.add(
              "layout",
              c("layout").get.mapObject(_.add("element", Json.fromString("float32")))
            )
          )
        )
      )
    )
    assert(archiveCodec.codec.decode(forged).isLeft)
  }

  test("missing chunk and unavailable recomputation are typed located failures") {
    val packed = doc(DensityStorage.Packed)._1.archive.densityReader().density(0, key)
    assert(packed.left.exists {
      case DensityError.MissingPayload(0, `key`, _) => true; case _ => false
    })
    val recomputable =
      doc(DensityStorage.Recomputable)._1.archive.densityReader().density(0, key)
    assertEquals(recomputable, Left(DensityError.RecomputeUnavailable(0, key)))
  }

  test("a different verified chunk is not accepted as the requested chunk") {
    val (bundle, _) = doc(DensityStorage.Packed)
    val wrong       = bundle.chunks.last._2
    val error       = bundle.archive.densityReader(_ => Some(wrong)).density(0, key)
    assert(error.left.exists {
      case DensityError.PayloadReference(0, `key`, _, _) => true; case _ => false
    })
  }

  test("unknown scales and keys remain explicit") {
    val reader = doc(DensityStorage.Inline)._1.archive.densityReader()
    assertEquals(reader.density(-1, key), Left(DensityError.UnknownScale(-1, 1)))
    val missing = StudyKey("absent", "absent", "recall")
    assertEquals(reader.density(0, missing), Left(DensityError.UnknownKey(0, missing)))
  }

  test("duplicate keys are ambiguous and stored failures remain failures") {
    val p = StudyResultFixtures.cosinePlan(StudyResultFixtures.mixed, FailurePolicy.RequireAll)
    val r = get(p.run(StudyResultFixtures.mixed))
    val bundle    = get(archiveCodec.encode(r, DensityStorage.Packed))
    val reader    = bundle.archive.densityReader(provider(bundle))
    val duplicate = StudyKey("p2", "a", "recall")
    assertEquals(reader.density(0, duplicate), Left(DensityError.AmbiguousKey(0, duplicate, 2)))
    val failed = StudyKey("p1", "b", "recall")
    assert(reader.density(0, failed).left.exists {
      case DensityError.Failed(0, `failed`, _) => true; case _ => false
    })
    assertEquals(
      get(results.codec.encode(get(bundle.archive.materialize(provider(bundle))))),
      get(results.codec.encode(r))
    )
  }

  test("recomputed cells must match the pinned per-map digest") {
    val (bundle, _) = doc(DensityStorage.Recomputable)
    val changed     =
      get(Surface.mass(mass.grid, IArray.from(mass.values.toVector.reverse), mass.provenance))
    val read =
      bundle.archive.densityReader(recompute = Some((_, _) => Right(changed))).density(0, key)
    assert(read.left.exists {
      case DensityError.DigestMismatch(0, `key`, _, _) => true; case _ => false
    })
  }

  test("declared provenance is checked even if a recomputer returns the original map") {
    val (_, document) = doc(DensityStorage.Recomputable)
    val forged        = changeOutcome(document)(
      _.mapObject(o =>
        o.add("provenance", o("provenance").get.mapObject(_.add("steps", Json.arr())))
      )
    )
    val opened = get(archiveCodec.codec.decode(forged))
    assert(opened.densityReader(recompute = Some((_, _) => Right(mass))).density(0, key).isLeft)
  }

  test("inconsistent geometry cannot be hidden by a correct recomputed map") {
    val (_, document) = doc(DensityStorage.Recomputable)
    val forged        = document.mapObject(o =>
      o.add(
        "value",
        o("value").get.mapObject(p =>
          p.add(
            "description",
            p("description").get.mapArray(_.map { field =>
              if field.hcursor.get[String]("field").contains("grid") then
                field.mapObject(_.add("values", Json.arr()))
              else field
            })
          )
        )
      )
    )
    val opened = get(archiveCodec.codec.decode(forged))
    assert(opened.densityReader(recompute = Some((_, _) => Right(mass))).density(0, key).isLeft)
  }

  test("source plan mismatch is refused before recomputation") {
    val (bundle, _) = doc(DensityStorage.Recomputable)
    val other       =
      StudyResultFixtures.cosinePlan(StudyResultFixtures.clean, FailurePolicy.RequireAll)
    assert(archiveCodec.recomputer(other, input, bundle.archive)(0, key).left.exists {
      case DensityError.SourceMismatch(0, `key`, _, _) => true
      case _                                           => false
    })
  }

  test(
    "lazy opening does not assert completed-result validity; materialization checks reductions"
  ) {
    val (bundle, document) = doc(DensityStorage.Inline)
    val forged             = document.mapObject(o =>
      o.add(
        "value",
        o("value").get.mapObject(_.add("input", Json.fromString("0000000000000000")))
      )
    )
    val opened = get(archiveCodec.codec.decode(forged))
    assert(opened.materialize(provider(bundle)).isLeft)
  }

  test("density identity parsing, equality and hashing preserve the canonical SHA") {
    val digest = get(DensityDigest.of(mass, result.description))
    val parsed = get(DensityDigest.parse[Px](digest.hex))
    assert(parsed.sameAs(digest))
    assertEquals(parsed, digest)
    assertEquals(parsed.hashCode, digest.hashCode)
    assertEquals(parsed.toString, s"sha256:${digest.hex}")
    assertEquals(parsed.sha256, parsed.canonical.sha256)
    assert(DensityDigest.parse[Px]("invalid").isLeft)
    assert(!digest.equals("not a density identity"))
  }

  test("declared scale metadata must agree with the digested saved description") {
    val (_, document) = doc(DensityStorage.Recomputable)
    val forged        = document.mapObject(o =>
      o.add(
        "value",
        o("value").get.mapObject(p =>
          p.add(
            "scales",
            p("scales").get.mapArray(
              _.map(
                _.mapObject(
                  _.add("estimate", Json.obj("kind" -> Json.fromString("unknown-estimator")))
                )
              )
            )
          )
        )
      )
    )
    assert(archiveCodec.codec.decode(forged).isLeft)
  }

  test("changed saved digest is refused even when recomputation succeeds") {
    val (_, document) = doc(DensityStorage.Recomputable)
    val forged = storage(document)(_.mapObject(_.add("digest", Json.fromString("f" * 64))))
    val opened = get(archiveCodec.codec.decode(forged))
    val error  = opened
      .densityReader(recompute = Some(archiveCodec.recomputer(plan, input, opened)))
      .density(0, key)
    assert(error.left.exists {
      case DensityError.DigestMismatch(0, `key`, expected, actual) =>
        expected.hex == "f" * 64 && actual.hex != expected.hex
      case _ => false
    })
  }

  test("lifting v1 and reencoding preserves member order and exact canonical bytes") {
    val document = get(results.codec.encode(result))
    val lifted   = get(archiveCodec.ladder.lift(document))
    val reopened = get(archiveCodec.codec.decode(lifted))
    assertEquals(get(archiveCodec.codec.encode(reopened)).noSpaces, document.noSpaces)
  }

  test("single-map recomputation reuses window, initial-fixation and all estimator policies") {
    val source = StudyResultFixtures.clean
    val parent = source.trials.rows.head.value.frame
    val window =
      get(Subframe.of(parent, FrameId("archive-window"), get(Bounds.of[Px](0.25, 0.25, 2, 2))))
    val grid     = get(Grid.over(window.frame, 2, 2))
    val geometry = get(StudyGeometry.windowed(window, grid, OffWindowPolicy.Exclude))
    val scales   = Vector[StudyEstimate[Px]](
      StudyEstimate.Binned(),
      StudyEstimate.Gaussian(get(Sigma.px(0.5)), EdgePolicy.Truncate),
      StudyEstimate.Anisotropic(get(Sigma.px(0.5)), get(Sigma.px(0.8)), EdgePolicy.Truncate)
    )
    val configured = get(
      StudyPlan.configure(
        source.reference,
        StudyKey.layout(DefinitionId.studyLayout),
        geometry,
        "recall",
        "encode",
        Weight.Duration,
        scales.map(StudyScale.Native(_)),
        Some(get(LinearAngularScale.of(parent, 2.0))),
        FailurePolicy.RequireAll,
        StudyMethod.cosine[Px](DefinitionId.cosine),
        (),
        initialFixations = InitialFixationPolicy.dropFirst
      )
    )
    val completed = get(configured.run(source))
    val bundle    = get(archiveCodec.encode(completed))
    val recompute = archiveCodec.recomputer(configured, source, bundle.archive)
    val restored  = get(bundle.archive.materialize(recompute = Some(recompute)))
    assertEquals(get(results.codec.encode(restored)), get(results.codec.encode(completed)))
    completed.scales.zipWithIndex.foreach { (scale, s) =>
      scale.estimation.foreach { (key, outcome) =>
        outcome.foreach { expected =>
          val view =
            get(bundle.archive.densityReader(recompute = Some(recompute)).density(s, key))
          assertEquals(view.cells.toVector, expected.values.toVector)
          assertEquals(view.geometry.origin, Pt[Px](0.25, 0.25))
          assertEquals(
            view.geometry.cellDegrees,
            Some(get(Extent.of[Unit2D.Deg](0.4375, 0.4375)))
          )
        }
      }
    }
    assert(completed.scales.forall(_.estimation.exists(_._2.isRight)))
  }
