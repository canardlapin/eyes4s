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

import cats.syntax.all.*
import eyes4s.design.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.DefinitionId
import io.circe.{ACursor, Json}

/** Every value has one written document: a decoder refuses a second spelling
  * that decodes to the same value, naming the member, what it found and what
  * the writer writes.
  */
class CanonicalWireSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)
  private def id(name: String): DefinitionId    = get(DefinitionId.of(name, 1))
  private def edit(json: Json, path: String*)(f: Json => Json): Json =
    path.foldLeft(json.hcursor: ACursor)(_.downField(_)).withFocus(f).top.get
  private def strings(values: String*): Json = Json.arr(values.map(Json.fromString)*)
  private val ascending                      = "members are written in ascending order"

  private val split: TemplateSplit[String] = get(
    for
      basis <- TemplateBasis.of("basis/1", Vector("a"), "score")
      rows <- Vector(("k1", "f1", 1.0), ("k2", "f2", 2.0), ("k3", "f3", 3.0), ("k4", "f1", 4.0))
        .traverse((k, f, x) => TemplateObservation.of(k, f, Vector(x), x))
      value <- TemplateSplit.of(basis, rows, Set("f2", "f3"))
    yield value
  )

  test("a template recipe's held-out folds are refused out of order") {
    val codec = TemplateRecipeCodec.of(id("test.recipe"), VersionedCodec.string(id("test.key")))
    val encoded = get(codec.encode(split))
    assertEquals(
      encoded.hcursor.downField("value").get[Json]("heldOutFolds"),
      Right(strings("f2", "f3"))
    )
    assert(codec.decode(encoded).isRight)
    val reversed = edit(encoded, "value", "heldOutFolds")(_ => strings("f3", "f2"))
    assertEquals(
      codec.decode(reversed),
      Left(
        CodecError.NonCanonical(
          "heldOutFolds",
          strings("f3", "f2"),
          strings("f2", "f3"),
          ascending
        )
      )
    )
    val native =
      NativeTemplateRecipeCodec.of(id("test.native"), VersionedCodec.string(id("test.key")))
    val nativeReversed =
      edit(get(native.encode(split)), "value", "heldOutFolds")(_ => strings("f3", "f2"))
    assert(native.decode(nativeReversed).left.exists {
      case CodecError.NonCanonical("heldOutFolds", _, _, _) => true
      case _                                                => false
    })
  }

  test("a typed map's entries are refused out of key order") {
    val codec = VersionedCodec.entries(
      id("test.map"),
      VersionedCodec.string(id("test.k")),
      VersionedCodec.string(id("test.v"))
    )
    val encoded = get(codec.encode(Map("a" -> "1", "b" -> "2")))
    val rows    = encoded.hcursor.downField("value").focus.flatMap(_.asArray).get
    val swapped = edit(encoded, "value")(_ => Json.arr(rows.reverse*))
    assertEquals(
      codec.decode(swapped),
      Left(
        CodecError.NonCanonical("entries", Json.arr(rows.reverse*), Json.arr(rows*), ascending)
      )
    )
  }

  test("temporal epochs and coverage intervals are refused out of order") {
    val codec   = TemporalInputCodecs.study[Px]()
    val encoded = get(codec.input.encode(InputPayloadFixtures.temporal))
    val epochs  =
      encoded.hcursor.downField("value").downField("epochs").focus.flatMap(_.asArray).get
    val swapped = edit(encoded, "value", "epochs")(_ => Json.arr((epochs.tail :+ epochs.head)*))
    assert(codec.input.decode(swapped).left.exists {
      case CodecError.NonCanonical("epochs", _, canonical, `ascending`) =>
        canonical == Json.arr(epochs*)
      case _ => false
    })
    val gapped = epochs.indexWhere(
      _.hcursor
        .downField("coverage")
        .downField("intervals")
        .focus
        .flatMap(_.asArray)
        .exists(_.size > 1)
    )
    assert(gapped >= 0, "the temporal fixture has an epoch with a coverage gap")
    val intervals =
      epochs(gapped).hcursor
        .downField("coverage")
        .downField("intervals")
        .focus
        .flatMap(_.asArray)
        .get
    val reordered = edit(encoded, "value", "epochs")(_ =>
      Json.arr(
        epochs.updated(
          gapped,
          edit(epochs(gapped), "coverage", "intervals")(_ => Json.arr(intervals.reverse*))
        )*
      )
    )
    assertEquals(
      codec.input.decode(reordered),
      Left(
        CodecError.Entry(
          s"epochs[$gapped]",
          CodecError.NonCanonical(
            "coverage.intervals",
            Json.arr(intervals.reverse*),
            Json.arr(intervals*),
            ascending
          )
        )
      )
    )
  }

  test("observed coverage refuses its intervals out of onset order") {
    val codec = DomainCodecs.coverage(id("test.coverage"))
    val clock = eyes4s.kernel.ClockId("c")
    val value = get(
      eyes4s.core.ObservedCoverage.of(
        clock,
        Vector(
          get(
            eyes4s.kernel.Interval
              .of(clock, eyes4s.kernel.Instant.micros(0), eyes4s.kernel.Instant.micros(5))
          ),
          get(
            eyes4s.kernel.Interval
              .of(clock, eyes4s.kernel.Instant.micros(9), eyes4s.kernel.Instant.micros(12))
          )
        )
      )
    )
    val encoded   = get(codec.encode(value))
    val intervals =
      encoded.hcursor.downField("value").downField("intervals").focus.flatMap(_.asArray).get
    val reversed = edit(encoded, "value", "intervals")(_ => Json.arr(intervals.reverse*))
    assertEquals(
      codec.decode(reversed),
      Left(
        CodecError.NonCanonical(
          "intervals",
          Json.arr(intervals.reverse*),
          Json.arr(intervals*),
          ascending
        )
      )
    )
  }

  test("an evaluation specification's parameters are refused out of name order") {
    val json = Json.obj(
      "method"     -> Json.fromString("m"),
      "revision"   -> Json.fromString("1"),
      "parameters" -> Json.arr(
        Json.obj(
          "name"  -> Json.fromString("b"),
          "kind"  -> Json.fromString("text"),
          "value" -> Json.fromString("x")
        ),
        Json.obj(
          "name"  -> Json.fromString("a"),
          "kind"  -> Json.fromString("text"),
          "value" -> Json.fromString("y")
        )
      )
    )
    assert(ResultWire.readEvaluationSpec[Px](json).left.exists {
      case CodecError.NonCanonical("parameters", _, canonical, `ascending`) =>
        canonical.asArray
          .exists(_.flatMap(_.hcursor.get[String]("name").toOption) == Vector("a", "b"))
      case _ => false
    })
  }

  test("microseconds are refused in any spelling but the plain decimal one") {
    val codec = DomainCodecs.instant(id("test.instant"))
    val plain = get(codec.encode(eyes4s.kernel.Instant.micros(5)))
    assertEquals(codec.decode(plain).map(_.toMicros), Right(5L))
    Vector("+5" -> "5", "05" -> "5", "-0" -> "0").foreach { (spelled, canonical) =>
      val other = edit(plain, "value")(_ => Json.fromString(spelled))
      assert(
        codec.decode(other).left.exists {
          case CodecError.NonCanonical(_, found, written, _) =>
            found == Json.fromString(spelled) && written == Json.fromString(canonical)
          case _ => false
        },
        spelled
      )
    }
  }

  test(
    "numbers are JSON numbers, integers are spelled as integers, and absence has one spelling"
  ) {
    final case class Probe(n: Int, x: Double, note: Option[String])
    val probe = VersionedCodec.of[Probe](id("test.probe"))(p =>
      Json.obj(
        "n"    -> Json.fromInt(p.n),
        "x"    -> Json.fromDoubleOrNull(p.x),
        "note" -> p.note.fold(Json.Null)(Json.fromString)
      )
    )(json =>
      for
        n    <- Wire.field[Int](json, "n")
        x    <- Wire.field[Double](json, "x")
        note <- Wire.field[Option[String]](json, "note")
      yield Probe(n, x, note)
    )
    val encoded = get(probe.encode(Probe(3, 1.5, None)))
    assertEquals(probe.decode(encoded), Right(Probe(3, 1.5, None)))
    def refusedAt(member: String)(change: Json => Json): Boolean =
      probe.decode(edit(encoded, "value", member)(change)).left.exists {
        case CodecError.Field(`member`, _, _) => true
        case _                                => false
      }
    assert(refusedAt("n")(_ => Json.fromString("3")), "a numeric string")
    assert(refusedAt("n")(_ => Json.fromDoubleOrNull(3.0)), "an integer spelled 3.0")
    assert(
      refusedAt("n")(_ => io.circe.parser.parse("3e0").toOption.get),
      "an integer spelled 3e0"
    )
    assert(refusedAt("x")(_ => Json.fromString("1.5")), "a numeric string")
    assert(refusedAt("x")(_ => Json.Null), "null for a number")
    val absent = encoded.hcursor.downField("value").downField("note").delete.top.get
    assert(probe.decode(absent).isLeft, "a member written as null may not be omitted")
  }

  test("a scanpath's omitted source may not be spelled null") {
    val studies = StudyInputCodecs.study[Px]
    val encoded = get(studies.input.encode(InputPayloadFixtures.temporalStudy))
    val path    = Vector("value", "trials", "value")
    val rows    =
      path.foldLeft(encoded.hcursor: ACursor)(_.downField(_)).focus.flatMap(_.asArray).get
    val nulled = edit(encoded, path*)(_ =>
      Json.arr(
        rows.updated(
          0,
          edit(rows(0), "value", "value")(_.mapObject(_.add("source", Json.Null)))
        )*
      )
    )
    assert(studies.input.decode(nulled).left.exists {
      case CodecError.Entry(_, CodecError.NonCanonical("source", _, _, _)) => true
      case _                                                               => false
    })
  }

  test("an identity table may not declare one identity twice") {
    val codec   = DomainCodecs.identities(id("test.identities"))
    val table   = DocumentIdentities.empty.addClock(eyes4s.kernel.ClockId("c"))
    val encoded = get(codec.encode(table))
    val twice   = edit(encoded, "value", "clocks")(_ => strings("c", "c"))
    assertEquals(
      codec.decode(twice),
      Left(
        CodecError.NonCanonical(
          "clocks",
          strings("c", "c"),
          strings("c"),
          "each identity is declared once"
        )
      )
    )
  }
