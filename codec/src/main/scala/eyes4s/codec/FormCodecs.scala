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
import eyes4s.kernel.{LengthUnit, PlanarUnit}
import eyes4s.plan.*
import io.circe.Json

/** The identities of the form documents a host stores (CR6c). */
object FormCodecDefinitions:
  /** A host's raw form values, keyed by field. */
  val formValues: DefinitionId = DefinitionId.builtIn("eyes4s.form-values", 1)

  /** The fields a method or recipe presents ([[FormView]]). */
  val formView: DefinitionId = DefinitionId.builtIn("eyes4s.form-view", 1)

/** Codecs for form values and form views (`eyes4s.form-values@1`,
  * `eyes4s.form-view@1`).
  *
  * A number stays the text the user typed, so an empty or malformed entry
  * survives a round trip exactly. Form values are written in ascending field
  * order, with no absent entry: [[FormValues]] holds none. A view decodes
  * through the plan's own constructors ([[FieldView.of]], [[NumericBounds.of]],
  * [[DefaultValue.of]]), so a stored view cannot hold a field the plan would
  * refuse. Every tag is a lower-camel token; units are their symbols.
  */
object FormCodecs:
  private def str(value: String): Json = Json.fromString(value)

  private def refused(path: String, json: Json)(error: DescriptorError): CodecError =
    CodecError.Field(path, json, error.message)

  private def tag(json: Json): Either[CodecError, String] = Wire.field[String](json, "kind")

  private def unknown[A](json: Json, path: String, token: String): Either[CodecError, A] =
    Left(CodecError.Field(path, json, s"unknown value $token"))

  private def fieldId(json: Json, name: String): Either[CodecError, FieldId] =
    Wire.field[String](json, name).flatMap(v => FieldId.of(v).left.map(refused(name, json)))

  // --- Raw values -----------------------------------------------------------

  private[codec] def raw(value: RawValue): Json = value match
    case RawValue.Number(text)  => Json.obj("kind" -> str("number"), "text" -> str(text))
    case RawValue.Choice(token) => Json.obj("kind" -> str("choice"), "token" -> str(token))
    case RawValue.Flag(flag)    =>
      Json.obj("kind" -> str("flag"), "value" -> Json.fromBoolean(flag))
    case RawValue.Text(text)     => Json.obj("kind" -> str("text"), "value" -> str(text))
    case RawValue.Absent         => Json.obj("kind" -> str("absent"))
    case RawValue.Group(fields)  => Json.obj("kind" -> str("group"), "fields" -> named(fields))
    case RawValue.Variant(t, fs) =>
      Json.obj("kind" -> str("variant"), "token" -> str(t), "fields" -> named(fs))
    case RawValue.Items(values) =>
      Json.obj("kind" -> str("items"), "values" -> Json.arr(values.map(raw)*))

  private def named(fields: Vector[(FieldId, RawValue)]): Json =
    Json.arr(fields.map((id, v) => Json.obj("id" -> str(id.value), "value" -> raw(v)))*)

  private[codec] def readRaw(json: Json): Either[CodecError, RawValue] =
    tag(json).flatMap {
      case "number"  => Wire.field[String](json, "text").map(RawValue.Number(_))
      case "choice"  => Wire.field[String](json, "token").map(RawValue.Choice(_))
      case "flag"    => Wire.field[Boolean](json, "value").map(RawValue.Flag(_))
      case "text"    => Wire.field[String](json, "value").map(RawValue.Text(_))
      case "absent"  => Right(RawValue.Absent)
      case "group"   => readNamed(json).map(RawValue.Group(_))
      case "variant" =>
        (Wire.field[String](json, "token"), readNamed(json)).mapN(RawValue.Variant(_, _))
      case "items" =>
        Wire
          .field[Vector[Json]](json, "values")
          .flatMap(
            _.zipWithIndex.traverse((v, i) => readRaw(v).left.map(Wire.at(s"values.$i")))
          )
          .map(RawValue.Items(_))
      case other => unknown(json, "kind", other)
    }

  private def readNamed(json: Json): Either[CodecError, Vector[(FieldId, RawValue)]] =
    Wire
      .field[Vector[Json]](json, "fields")
      .flatMap(_.zipWithIndex.traverse { (entry, i) =>
        (fieldId(entry, "id"), Wire.field[Json](entry, "value").flatMap(readRaw))
          .mapN((_, _))
          .left
          .map(Wire.at(s"fields.$i"))
      })

  /** `eyes4s.form-values@1`: `{"fields": [{"id", "value"}]}` in ascending
    * field order. A duplicate field or an absent value is refused.
    */
  val values: VersionedCodec[FormValues] =
    VersionedCodec.of[FormValues](FormCodecDefinitions.formValues)(v =>
      Json.obj(
        "fields" -> named(v.values.toVector.sortBy(_._1.value))
      )
    )(json =>
      for
        entries <- readNamed(json)
        _       <- Wire.ascending(
          "fields",
          entries.map((id, v) => id.value -> Json.obj("id" -> str(id.value), "value" -> raw(v)))
        )
        _ <- duplicate(json, entries.map(_._1)).toLeft(())
        _ <- entries
          .collectFirst { case (id, RawValue.Absent) =>
            CodecError.Field("fields", json, s"field ${id.value} is absent; leave it out")
          }
          .toLeft(())
      yield FormValues.from(entries.toMap)
    )

  // --- Views ------------------------------------------------------------------

  private def planar(unit: PlanarUnit): Json = str(unit.symbol)
  private def readPlanar(json: Json, name: String): Either[CodecError, PlanarUnit] =
    Wire.field[String](json, name).flatMap { s =>
      PlanarUnit.values
        .find(_.symbol == s)
        .toRight(CodecError.Field(name, json, s"unknown unit $s"))
    }
  private def readLength(json: Json, name: String): Either[CodecError, LengthUnit] =
    Wire.field[String](json, name).flatMap { s =>
      LengthUnit.values
        .find(_.symbol == s)
        .toRight(CodecError.Field(name, json, s"unknown unit $s"))
    }

  private def lower(token: String): String = token.head.toLower +: token.tail

  private def quantity(q: Quantity): Json = q match
    case Quantity.Planar(u)         => Json.obj("kind" -> str("planar"), "unit" -> planar(u))
    case Quantity.Rate(u)           => Json.obj("kind" -> str("rate"), "unit" -> planar(u))
    case Quantity.UnitsPerDegree(u) =>
      Json.obj("kind" -> str("unitsPerDegree"), "unit" -> planar(u))
    case Quantity.Length(u) => Json.obj("kind" -> str("length"), "unit" -> str(u.symbol))
    case Quantity.Duration  => Json.obj("kind" -> str("duration"))
    case Quantity.Count(of) => Json.obj("kind" -> str("count"), "of" -> str(lower(of.toString)))
    case Quantity.Dimensionless => Json.obj("kind" -> str("dimensionless"))
    case Quantity.Nominal       => Json.obj("kind" -> str("nominal"))
    case Quantity.Composite     => Json.obj("kind" -> str("composite"))

  private def readQuantity(json: Json): Either[CodecError, Quantity] =
    tag(json).flatMap {
      case "planar"         => readPlanar(json, "unit").map(Quantity.Planar(_))
      case "rate"           => readPlanar(json, "unit").map(Quantity.Rate(_))
      case "unitsPerDegree" => readPlanar(json, "unit").map(Quantity.UnitsPerDegree(_))
      case "length"         => readLength(json, "unit").map(Quantity.Length(_))
      case "duration"       => Right(Quantity.Duration)
      case "count"          =>
        Wire.field[String](json, "of").flatMap { s =>
          Counted.values
            .find(c => lower(c.toString) == s)
            .map(Quantity.Count(_))
            .toRight(
              CodecError.Field("of", json, s"unknown value $s")
            )
        }
      case "dimensionless" => Right(Quantity.Dimensionless)
      case "nominal"       => Right(Quantity.Nominal)
      case "composite"     => Right(Quantity.Composite)
      case other           => unknown(json, "kind", other)
    }

  private def shape(s: NumberShape): Json                            = str(lower(s.toString))
  private def readShape(json: Json): Either[CodecError, NumberShape] =
    Wire.field[String](json, "shape").flatMap { s =>
      NumberShape.values
        .find(v => lower(v.toString) == s)
        .toRight(
          CodecError.Field("shape", json, s"unknown value $s")
        )
    }

  private def endpoint(e: Endpoint): Json = e match
    case Endpoint.Open(v) =>
      Json.obj("kind" -> str("open"), "value" -> Json.fromDoubleOrNull(v))
    case Endpoint.Closed(v) =>
      Json.obj("kind" -> str("closed"), "value" -> Json.fromDoubleOrNull(v))

  private def readEndpoint(json: Json): Either[CodecError, Endpoint] =
    (tag(json), Wire.field[Double](json, "value")).tupled.flatMap {
      case ("open", v)   => Right(Endpoint.Open(v))
      case ("closed", v) => Right(Endpoint.Closed(v))
      case (other, _)    => unknown(json, "kind", other)
    }

  private def bounds(b: NumericBounds): Json =
    Json.obj(
      "lower" -> b.lower.fold(Json.Null)(endpoint),
      "upper" -> b.upper.fold(Json.Null)(endpoint)
    )

  private def readBounds(json: Json): Either[CodecError, NumericBounds] =
    for
      l <- Wire.field[Option[Json]](json, "lower").flatMap(_.traverse(readEndpoint))
      u <- Wire.field[Option[Json]](json, "upper").flatMap(_.traverse(readEndpoint))
      b <- NumericBounds.of(l, u).left.map(refused("bounds", json))
    yield b

  private def option(o: ChoiceOption): Json =
    Json.obj("token" -> str(o.token), "label" -> str(o.label))

  private def choices(source: ChoiceSource): Json = source match
    case ChoiceSource.Fixed(options) =>
      Json.obj("kind" -> str("fixed"), "options" -> Json.arr(options.map(option)*))
    case ChoiceSource.FromInput(domain) =>
      Json.obj("kind" -> str("fromInput"), "domain" -> str(lower(domain.toString)))

  private def readChoices(json: Json): Either[CodecError, ChoiceSource] =
    tag(json).flatMap {
      case "fixed" =>
        Wire
          .field[Vector[Json]](json, "options")
          .flatMap(
            _.traverse(o =>
              (Wire.field[String](o, "token"), Wire.field[String](o, "label"))
                .mapN(ChoiceOption(_, _))
            )
          )
          .map(ChoiceSource.Fixed(_))
      case "fromInput" =>
        Wire.field[String](json, "domain").flatMap { s =>
          InputDomain.values
            .find(d => lower(d.toString) == s)
            .map(ChoiceSource.FromInput(_))
            .toRight(
              CodecError.Field("domain", json, s"unknown value $s")
            )
        }
      case other => unknown(json, "kind", other)
    }

  private def rule(r: GroupRule): Json = r match
    case GroupRule.Independent    => Json.obj("kind" -> str("independent"))
    case GroupRule.Ordered(pairs) =>
      Json.obj(
        "kind"  -> str("ordered"),
        "pairs" -> Json.arr(pairs.map((a, b) => Json.arr(str(a.value), str(b.value)))*)
      )
    case GroupRule.Distinct(parts) =>
      Json.obj("kind" -> str("distinct"), "parts" -> Json.arr(parts.map(p => str(p.value))*))

  private def readId(json: Json, path: String, value: String): Either[CodecError, FieldId] =
    FieldId.of(value).left.map(refused(path, json))

  private def readRule(json: Json): Either[CodecError, GroupRule] =
    tag(json).flatMap {
      case "independent" => Right(GroupRule.Independent)
      case "ordered"     =>
        Wire
          .field[Vector[Vector[String]]](json, "pairs")
          .flatMap(_.traverse {
            case Vector(a, b) => (readId(json, "pairs", a), readId(json, "pairs", b)).tupled
            case other        =>
              Left(CodecError.Field("pairs", json, s"expected a pair, got ${other.size} ids"))
          })
          .map(GroupRule.Ordered(_))
      case "distinct" =>
        Wire
          .field[Vector[String]](json, "parts")
          .flatMap(_.traverse(readId(json, "parts", _)))
          .map(GroupRule.Distinct(_))
      case other => unknown(json, "kind", other)
    }

  private def kind(k: FieldKind): Json = k match
    case FieldKind.Numeric(q, s, b) =>
      Json.obj(
        "kind"     -> str("numeric"),
        "quantity" -> quantity(q),
        "shape"    -> shape(s),
        "bounds"   -> bounds(b)
      )
    case FieldKind.Choice(source) =>
      Json.obj("kind" -> str("choice"), "options" -> choices(source))
    case FieldKind.Toggle          => Json.obj("kind" -> str("toggle"))
    case FieldKind.Text            => Json.obj("kind" -> str("text"))
    case FieldKind.Reference       => Json.obj("kind" -> str("reference"))
    case FieldKind.Group(parts, r) =>
      Json.obj(
        "kind"  -> str("group"),
        "parts" -> Json.arr(parts.map(field)*),
        "rule"  -> rule(r)
      )
    case FieldKind.Optional(of, absentMeans) =>
      Json.obj("kind" -> str("optional"), "of" -> field(of), "absentMeans" -> str(absentMeans))
    case FieldKind.Repeated(of, minimum, maximum) =>
      Json.obj(
        "kind"    -> str("repeated"),
        "of"      -> field(of),
        "minimum" -> Json.fromInt(minimum),
        "maximum" -> maximum.fold(Json.Null)(Json.fromInt)
      )
    case FieldKind.Variant(cases) =>
      Json.obj(
        "kind"  -> str("variant"),
        "cases" -> Json.arr(cases.map { c =>
          Json.obj(
            "token" -> str(c.token),
            "label" -> str(c.label),
            "parts" -> Json.arr(c.parts.map(field)*)
          )
        }*)
      )

  private def readParts(json: Json, name: String): Either[CodecError, Vector[FieldView]] =
    Wire
      .field[Vector[Json]](json, name)
      .flatMap(_.zipWithIndex.traverse((p, i) => readField(p).left.map(Wire.at(s"$name.$i"))))

  private def readKind(json: Json): Either[CodecError, FieldKind] =
    tag(json).flatMap {
      case "numeric" =>
        for
          q <- Wire.field[Json](json, "quantity").flatMap(readQuantity)
          s <- readShape(json)
          b <- Wire.field[Json](json, "bounds").flatMap(readBounds)
        yield FieldKind.Numeric(q, s, b)
      case "choice" =>
        Wire.field[Json](json, "options").flatMap(readChoices).map(FieldKind.Choice(_))
      case "toggle"    => Right(FieldKind.Toggle)
      case "text"      => Right(FieldKind.Text)
      case "reference" => Right(FieldKind.Reference)
      case "group"     =>
        (readParts(json, "parts"), Wire.field[Json](json, "rule").flatMap(readRule))
          .mapN(FieldKind.Group(_, _))
      case "optional" =>
        (
          Wire.field[Json](json, "of").flatMap(readField).left.map(Wire.at("of")),
          Wire.field[String](json, "absentMeans")
        ).mapN(FieldKind.Optional(_, _))
      case "repeated" =>
        (
          Wire.field[Json](json, "of").flatMap(readField).left.map(Wire.at("of")),
          Wire.field[Int](json, "minimum"),
          Wire.field[Option[Int]](json, "maximum")
        ).mapN(FieldKind.Repeated(_, _, _))
      case "variant" =>
        Wire
          .field[Vector[Json]](json, "cases")
          .flatMap(_.zipWithIndex.traverse { (c, i) =>
            (
              Wire.field[String](c, "token"),
              Wire.field[String](c, "label"),
              readParts(c, "parts")
            ).mapN(VariantCase(_, _, _)).left.map(Wire.at(s"cases.$i"))
          })
          .map(FieldKind.Variant(_))
      case other => unknown(json, "kind", other)
    }

  private[codec] def field(view: FieldView): Json =
    Json.obj(
      "id"      -> str(view.id.value),
      "version" -> Json.fromInt(view.version),
      "meaning" -> str(view.meaning),
      "kind"    -> kind(view.kind),
      "default" -> view.default.fold(Json.Null)(d =>
        Json.obj("raw" -> raw(d.raw), "reason" -> str(d.reason))
      )
    )

  private[codec] def readField(json: Json): Either[CodecError, FieldView] =
    for
      id      <- fieldId(json, "id")
      version <- Wire.field[Int](json, "version")
      meaning <- Wire.field[String](json, "meaning")
      k       <- Wire.field[Json](json, "kind").flatMap(readKind).left.map(Wire.at(id.value))
      default <- Wire
        .field[Option[Json]](json, "default")
        .flatMap(_.traverse { d =>
          for
            r      <- Wire.field[Json](d, "raw").flatMap(readRaw)
            reason <- Wire.field[String](d, "reason")
            value  <- DefaultValue.of(r, reason).left.map(refused("default", d))
          yield value
        })
      view <- FieldView.of(id, version, meaning, k, default).left.map(refused(id.value, json))
    yield view

  private def duplicate(json: Json, ids: Vector[FieldId]): Option[CodecError] =
    ids.groupBy(identity).collectFirst {
      case (id, all) if all.size > 1 =>
        CodecError.Field("fields", json, s"field ${id.value} appears ${all.size} times")
    }

  /** `eyes4s.form-view@1`: the definition and its fields, in the view's order.
    * Field ids are distinct; a view with a repeated id is refused on both
    * sides.
    */
  val view: VersionedCodec[FormView] =
    VersionedCodec.checked[FormView](FormCodecDefinitions.formView) { v =>
      val json = Json.obj(
        "definition" -> Wire.id(v.definition),
        "fields"     -> Json.arr(v.fields.map(field)*)
      )
      duplicate(json, v.fields.map(_.id)).toLeft(json)
    }(json =>
      for
        definition <- Wire.definition(json, "definition")
        fields     <- readParts(json, "fields")
        _          <- duplicate(json, fields.map(_.id)).toLeft(())
      yield FormView(definition, fields)
    )
