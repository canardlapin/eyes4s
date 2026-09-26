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
import eyes4s.plan.*
import eyes4s.results.*
import io.circe.Json

/** Built-in identities of the report documents. */
object ReportCodecDefinitions:
  /** A declaration of trial covariates and their types. */
  val covariateSchema: DefinitionId = DefinitionId.builtIn("eyes4s.covariate-schema", 1)

  /** A report specification. */
  val reportSpec: DefinitionId = DefinitionId.builtIn("eyes4s.report-spec", 1)

  /** An evaluated report with its binding. */
  val report: DefinitionId = DefinitionId.builtIn("eyes4s.report", 1)

/** Versioned codecs of covariate schemas, report specifications and reports,
  * and the binding of a report to the canonical digests of what it was
  * evaluated over.
  *
  * Every member is written in one canonical spelling: an absent filter,
  * contrast, standard error or covariate digest is `null`; numbers are JSON
  * numbers; roles, spreads, comparisons and window measures are kebab-case
  * tokens. Decoding rebuilds each value through its smart constructor, so a
  * document that describes an invalid specification, or a report whose
  * accounting does not add up, is refused.
  */
object ReportCodecs:
  import ReportCodecDefinitions as D

  // ------------------------------------------------------------------ binding

  /** The binding form of a canonical digest. */
  def digest[A](value: CanonicalDigest[A], field: String): Either[CodecError, BindingDigest] =
    BindingDigest.parse(field, value.sha256.hex).left.map(CodecError.Report.apply)

  /** A report's binding: the canonical digests of the plan, input and
    * result documents, and of the covariate source (the admission ledger
    * that carries the trials table) when the report reads covariates.
    */
  def binding[P, I, R, C](
      plan: (VersionedCodec[P], P),
      input: (VersionedCodec[I], I),
      result: (VersionedCodec[R], R),
      covariates: Option[(VersionedCodec[C], C)]
  ): Either[CodecError, ReportBinding] =
    def of[A](pair: (VersionedCodec[A], A), field: String) =
      pair._1.digest(pair._2).flatMap(digest(_, field))
    for
      p <- of(plan, "plan")
      i <- of(input, "input")
      r <- of(result, "result")
      c <- covariates.traverse(of(_, "covariates"))
    yield ReportBinding(p, i, r, c)

  // ------------------------------------------------------------------ tokens

  private def kebab(name: String): String =
    name.flatMap(c => if c.isUpper then "-" + c.toLower else c.toString).stripPrefix("-")

  private def token[A](path: String, values: Array[A], json: Json, found: String)(
      name: A => String
  ): Either[CodecError, A] =
    values
      .find(v => name(v) == found)
      .toRight(CodecError.Field(path, json, s"unknown value '$found'"))

  private def enumField[A](json: Json, field: String, values: Array[A])(
      name: A => String
  ): Either[CodecError, A] =
    Wire.field[String](json, field).flatMap(token(field, values, json, _)(name))

  private def role(r: Role): String               = kebab(r.toString)
  private def readRole(json: Json, field: String) =
    enumField(json, field, Role.values)(role)
  private def measure(m: WindowMeasure): String = kebab(m.toString)

  private def spec[A](path: String)(e: Either[SpecError, A]) =
    e.left.map(error => Wire.at(path)(CodecError.ReportSpec(error)))
  private def covariate[A](path: String)(e: Either[CovariateError[Nothing], A]) =
    e.left.map(error => Wire.at(path)(CodecError.Covariates(error)))

  // ------------------------------------------------------------------ covariates

  private def covariateType(kind: CovariateType): Json = kind match
    case CovariateType.Numeric(unit) =>
      Json.obj("kind" -> Json.fromString("numeric"), "unit" -> Json.fromString(unit.symbol))
    case CovariateType.Ordinal(levels) =>
      Json.obj(
        "kind"   -> Json.fromString("ordinal"),
        "levels" -> Json.arr(levels.values.map(Json.fromString)*)
      )
    case CovariateType.Categorical(levels) =>
      Json.obj(
        "kind"   -> Json.fromString("categorical"),
        "levels" -> Json.arr(levels.values.map(Json.fromString)*)
      )
    case CovariateType.Binary => Json.obj("kind" -> Json.fromString("binary"))

  private def readLevels(json: Json): Either[CodecError, Levels] =
    Wire.field[Vector[String]](json, "levels").flatMap(v => covariate("levels")(Levels.of(v)))

  private def readCovariateType(json: Json): Either[CodecError, CovariateType] =
    Wire.field[String](json, "kind").flatMap {
      case "numeric" =>
        Wire
          .field[String](json, "unit")
          .flatMap(u => covariate("unit")(NumericUnit.of(u)))
          .map(CovariateType.Numeric(_))
      case "ordinal"     => readLevels(json).map(CovariateType.Ordinal(_))
      case "categorical" => readLevels(json).map(CovariateType.Categorical(_))
      case "binary"      => Right(CovariateType.Binary)
      case other => Left(CodecError.Field("kind", json, s"unknown covariate type '$other'"))
    }

  private def readName(json: Json, field: String): Either[CodecError, CovariateName] =
    Wire.field[String](json, field).flatMap(n => covariate(field)(CovariateName.of(n)))

  private def covariatesJson(schema: CovariateSchema): Json =
    Json.obj("covariates" -> Json.arr(schema.covariates.map { c =>
      Json.obj("name" -> Json.fromString(c.name.value), "type" -> covariateType(c.kind))
    }*))

  private def readCovariates(json: Json): Either[CodecError, CovariateSchema] =
    for
      entries <- Wire.field[Vector[Json]](json, "covariates")
      read    <- entries.zipWithIndex.traverse { (entry, i) =>
        (for
          name <- readName(entry, "name")
          kind <- Wire.field[Json](entry, "type").flatMap(readCovariateType)
        yield Covariate(name, kind)).left.map(Wire.at(s"covariates[$i]"))
      }
      schema <- covariate("covariates")(CovariateSchema.of(read))
    yield schema

  /** `eyes4s.covariate-schema@1`: the declared covariates, in order. */
  val covariates: VersionedCodec[CovariateSchema] =
    VersionedCodec.checked[CovariateSchema](D.covariateSchema)(s => Right(covariatesJson(s)))(
      readCovariates
    )

  // ------------------------------------------------------------------ terms

  private def term(t: Term): Json = t match
    case NumericTerm.Occurrence            => Json.obj("kind" -> Json.fromString("occurrence"))
    case NumericTerm.Covariate(name, unit) =>
      Json.obj(
        "kind" -> Json.fromString("numeric-covariate"),
        "name" -> Json.fromString(name.value),
        "unit" -> Json.fromString(unit.symbol)
      )
    case NumericTerm.Window(m) =>
      Json.obj("kind" -> Json.fromString("window"), "measure" -> Json.fromString(measure(m)))
    case LevelTerm.Layout(field) =>
      Json.obj(
        "kind"  -> Json.fromString("layout"),
        "field" -> Json.fromString(kebab(field.toString))
      )
    case LevelTerm.Categorical(name, levels) =>
      Json.obj(
        "kind"   -> Json.fromString("categorical-covariate"),
        "name"   -> Json.fromString(name.value),
        "levels" -> Json.arr(levels.values.map(Json.fromString)*)
      )
    case LevelTerm.Ordinal(name, levels) =>
      Json.obj(
        "kind"   -> Json.fromString("ordinal-covariate"),
        "name"   -> Json.fromString(name.value),
        "levels" -> Json.arr(levels.values.map(Json.fromString)*)
      )
    case FlagTerm.Binary(name) =>
      Json.obj(
        "kind" -> Json.fromString("binary-covariate"),
        "name" -> Json.fromString(name.value)
      )

  private def readTerm(json: Json): Either[CodecError, Term] =
    Wire.field[String](json, "kind").flatMap {
      case "occurrence"        => Right(NumericTerm.Occurrence)
      case "numeric-covariate" =>
        for
          name <- readName(json, "name")
          unit <- Wire
            .field[String](json, "unit")
            .flatMap(u => covariate("unit")(NumericUnit.of(u)))
        yield NumericTerm.Covariate(name, unit)
      case "window" =>
        enumField(json, "measure", WindowMeasure.values)(measure).map(NumericTerm.Window(_))
      case "layout" =>
        enumField(json, "field", LayoutField.values)(f => kebab(f.toString))
          .map(LevelTerm.Layout(_))
      case "categorical-covariate" =>
        (readName(json, "name"), readLevels(json)).mapN(LevelTerm.Categorical(_, _))
      case "ordinal-covariate" =>
        (readName(json, "name"), readLevels(json)).mapN(LevelTerm.Ordinal(_, _))
      case "binary-covariate" => readName(json, "name").map(FlagTerm.Binary(_))
      case other              => Left(CodecError.Field("kind", json, s"unknown term '$other'"))
    }

  private def typed[A](json: Json, field: String)(
      pick: PartialFunction[Term, A],
      expected: String
  ): Either[CodecError, A] =
    Wire
      .field[Json](json, field)
      .flatMap(j => readTerm(j).left.map(Wire.at(field)))
      .flatMap(t => pick.lift(t).toRight(CodecError.Field(field, json, s"expected $expected")))

  private def numeric(json: Json, field: String) =
    typed(json, field)({ case t: NumericTerm => t }, "a numeric term")
  private def level(json: Json, field: String) =
    typed(json, field)({ case t: LevelTerm => t }, "a level term")
  private def flag(json: Json, field: String) =
    typed(json, field)({ case t: FlagTerm => t }, "a flag term")
  private def ordinal(json: Json, field: String) =
    typed(json, field)({ case t @ LevelTerm.Ordinal(_, _) => t }, "an ordinal term")

  // ------------------------------------------------------------------ predicates

  private def predicate(p: Predicate): Json = p match
    case Predicate.Cmp(t, comparison, threshold) =>
      Json.obj(
        "kind"       -> Json.fromString("cmp"),
        "term"       -> term(t),
        "comparison" -> Json.fromString(kebab(comparison.toString)),
        "threshold"  -> Json.fromDoubleOrNull(threshold)
      )
    case Predicate.In(t, levels) =>
      Json.obj(
        "kind"   -> Json.fromString("in"),
        "term"   -> term(t),
        "levels" -> Json.arr(levels.map(Json.fromString)*)
      )
    case Predicate.AtLeast(t, l) =>
      Json.obj(
        "kind"  -> Json.fromString("at-least"),
        "term"  -> term(t),
        "level" -> Json.fromString(l)
      )
    case Predicate.Is(t, v) =>
      Json.obj(
        "kind"  -> Json.fromString("is"),
        "term"  -> term(t),
        "value" -> Json.fromBoolean(v)
      )
    case Predicate.IsMissing(t) =>
      Json.obj("kind" -> Json.fromString("is-missing"), "term" -> term(t))
    case Predicate.And(l, r) =>
      Json.obj(
        "kind"  -> Json.fromString("and"),
        "left"  -> predicate(l),
        "right" -> predicate(r)
      )
    case Predicate.Or(l, r) =>
      Json.obj("kind" -> Json.fromString("or"), "left" -> predicate(l), "right" -> predicate(r))
    case Predicate.Not(i) => Json.obj("kind" -> Json.fromString("not"), "inner" -> predicate(i))

  private def readPredicate(json: Json): Either[CodecError, Predicate] =
    def nested(field: String) =
      Wire.field[Json](json, field).flatMap(j => readPredicate(j).left.map(Wire.at(field)))
    Wire.field[String](json, "kind").flatMap {
      case "cmp" =>
        for
          t          <- numeric(json, "term")
          comparison <- enumField(json, "comparison", Comparison.values)(c => kebab(c.toString))
          threshold  <- Wire.field[Double](json, "threshold")
        yield Predicate.Cmp(t, comparison, threshold)
      case "in" =>
        (level(json, "term"), Wire.field[Vector[String]](json, "levels"))
          .mapN(Predicate.In(_, _))
      case "at-least" =>
        (ordinal(json, "term"), Wire.field[String](json, "level")).mapN(Predicate.AtLeast(_, _))
      case "is" =>
        (flag(json, "term"), Wire.field[Boolean](json, "value")).mapN(Predicate.Is(_, _))
      case "is-missing" =>
        Wire.field[Json](json, "term").flatMap(readTerm).map(Predicate.IsMissing(_))
      case "and" => (nested("left"), nested("right")).mapN(Predicate.And(_, _))
      case "or"  => (nested("left"), nested("right")).mapN(Predicate.Or(_, _))
      case "not" => nested("inner").map(Predicate.Not(_))
      case other => Left(CodecError.Field("kind", json, s"unknown predicate '$other'"))
    }

  // ------------------------------------------------------------------ groupings

  private def bin(b: Bin): Json = Json.obj(
    "label" -> Json.fromString(b.label),
    "from"  -> Json.fromDoubleOrNull(b.from),
    "until" -> Json.fromDoubleOrNull(b.until),
    "upper" -> Json.fromString(kebab(b.upper.toString))
  )

  private def grouping(g: Grouping): Json = g match
    case Grouping.ByLevel(t) =>
      Json.obj("kind" -> Json.fromString("by-level"), "term" -> term(t))
    case Grouping.ByFlag(t) => Json.obj("kind" -> Json.fromString("by-flag"), "term" -> term(t))
    case Grouping.ByBins(t, bins) =>
      Json.obj(
        "kind" -> Json.fromString("by-bins"),
        "term" -> term(t),
        "bins" -> Json.arr(bins.bins.map(bin)*)
      )

  private def readGrouping(json: Json): Either[CodecError, Grouping] =
    Wire.field[String](json, "kind").flatMap {
      case "by-level" => level(json, "term").map(Grouping.ByLevel(_))
      case "by-flag"  => flag(json, "term").map(Grouping.ByFlag(_))
      case "by-bins"  =>
        for
          t    <- numeric(json, "term")
          raw  <- Wire.field[Vector[Json]](json, "bins")
          read <- raw.zipWithIndex.traverse { (b, i) =>
            (for
              label <- Wire.field[String](b, "label")
              from  <- Wire.field[Double](b, "from")
              until <- Wire.field[Double](b, "until")
              upper <- enumField(b, "upper", UpperEdge.values)(u => kebab(u.toString))
              made  <- spec(s"bins[$i]")(Bin.of(label, from, until, upper))
            yield made).left.map(Wire.at(s"bins[$i]"))
          }
          bins <- spec("bins")(Bins.of(read))
        yield Grouping.ByBins(t, bins)
      case other => Left(CodecError.Field("kind", json, s"unknown grouping '$other'"))
    }

  // ------------------------------------------------------------------ specification

  private def specJson(s: ReportSpec): Json = Json.obj(
    "id"         -> Json.fromString(s.id.value),
    "scale"      -> Json.fromInt(s.scale),
    "roles"      -> Json.arr(s.selection.roles.map(r => Json.fromString(role(r)))*),
    "components" -> Json.arr(s.selection.components.map(Json.fromString)*),
    "filter"     -> s.filter.fold(Json.Null)(predicate),
    "groupBy"    -> Json.arr(s.groupBy.map(grouping)*),
    "reduce"     -> (s.reduce match
      case ReducePolicy.ParticipantMeans(m) =>
        Json.obj(
          "kind"           -> Json.fromString("participant-means"),
          "minimumQueries" -> Json.fromInt(m.value)
        )
      case ReducePolicy.PooledQueries => Json.obj("kind" -> Json.fromString("pooled-queries"))),
    "contrast" -> s.contrast.fold(Json.Null)(c =>
      Json.obj(
        "term"       -> term(c.term),
        "minuend"    -> Json.fromString(c.minuend),
        "subtrahend" -> Json.fromString(c.subtrahend)
      )
    ),
    "spread" -> Json.fromString(kebab(s.spread.toString))
  )

  private def readSpec(json: Json): Either[CodecError, ReportSpec] =
    for
      id         <- Wire.field[String](json, "id").flatMap(i => spec("id")(ReportId.of(i)))
      scale      <- Wire.field[Int](json, "scale")
      roleNames  <- Wire.field[Vector[String]](json, "roles")
      roles      <- roleNames.traverse(token("roles", Role.values, json, _)(role))
      components <- Wire.field[Vector[String]](json, "components")
      selection  <- spec("roles")(ReportSelection.of(roles, components))
      filter     <- Wire
        .field[Option[Json]](json, "filter")
        .flatMap(_.traverse(f => readPredicate(f).left.map(Wire.at("filter"))))
      groups  <- Wire.field[Vector[Json]](json, "groupBy")
      groupBy <- groups.zipWithIndex.traverse((g, i) =>
        readGrouping(g).left.map(Wire.at(s"groupBy[$i]"))
      )
      reduceJson <- Wire.field[Json](json, "reduce")
      reduce     <- Wire.field[String](reduceJson, "kind").flatMap {
        case "participant-means" =>
          Wire
            .field[Int](reduceJson, "minimumQueries")
            .flatMap(m => spec("reduce.minimumQueries")(MinimumQueries.of(m)))
            .map(ReducePolicy.ParticipantMeans(_))
        case "pooled-queries" => Right(ReducePolicy.PooledQueries)
        case other            =>
          Left(CodecError.Field("reduce.kind", reduceJson, s"unknown policy '$other'"))
      }
      contrast <- Wire
        .field[Option[Json]](json, "contrast")
        .flatMap(_.traverse { c =>
          (for
            t          <- Wire.field[Json](c, "term").flatMap(readTerm)
            minuend    <- Wire.field[String](c, "minuend")
            subtrahend <- Wire.field[String](c, "subtrahend")
          yield LevelContrast(t, minuend, subtrahend)).left.map(Wire.at("contrast"))
        })
      spread <- enumField(json, "spread", Spread.values)(s => kebab(s.toString))
      made   <- spec("spec")(
        ReportSpec.of(id, scale, selection, filter, groupBy, reduce, contrast, spread)
      )
    yield made

  /** `eyes4s.report-spec@1`. */
  val reportSpec: VersionedCodec[ReportSpec] =
    VersionedCodec.checked[ReportSpec](D.reportSpec)(s => Right(specJson(s)))(readSpec)

  // ------------------------------------------------------------------ values

  private def undefined(r: UndefinedReason): Json = r match
    case UndefinedReason.TooFewForSpread(n) =>
      Json.obj("kind" -> Json.fromString("too-few-for-spread"), "n" -> Json.fromInt(n))
    case UndefinedReason.ZeroDuration => Json.obj("kind" -> Json.fromString("zero-duration"))
    case UndefinedReason.NotFinite(operation, n) =>
      Json.obj(
        "kind"      -> Json.fromString("not-finite"),
        "operation" -> Json.fromString(operation),
        "n"         -> Json.fromInt(n)
      )

  private def readUndefined(json: Json): Either[CodecError, UndefinedReason] =
    Wire.field[String](json, "kind").flatMap {
      case "too-few-for-spread" =>
        Wire.field[Int](json, "n").map(UndefinedReason.TooFewForSpread(_))
      case "zero-duration" => Right(UndefinedReason.ZeroDuration)
      case "not-finite"    =>
        (Wire.field[String](json, "operation"), Wire.field[Int](json, "n"))
          .mapN(UndefinedReason.NotFinite(_, _))
      case other => Left(CodecError.Field("kind", json, s"unknown reason '$other'"))
    }

  private def absence(a: Absence): Json = a match
    case Absence.NotRecorded           => Json.obj("kind" -> Json.fromString("not-recorded"))
    case Absence.Unparsed              => Json.obj("kind" -> Json.fromString("unparsed"))
    case Absence.Failed(code, message) =>
      Json.obj(
        "kind"    -> Json.fromString("failed"),
        "code"    -> Json.fromString(code.render),
        "message" -> Json.fromString(message)
      )
    case Absence.Undefined(reason) =>
      Json.obj("kind" -> Json.fromString("undefined"), "reason" -> undefined(reason))
    case Absence.EmptyGroup => Json.obj("kind" -> Json.fromString("empty-group"))
    case Absence.Unpaired   => Json.obj("kind" -> Json.fromString("unpaired"))
    case Absence.BelowMinimum(queries, required) =>
      Json.obj(
        "kind"     -> Json.fromString("below-minimum"),
        "queries"  -> Json.fromInt(queries),
        "required" -> Json.fromInt(required)
      )

  private def readAbsence(json: Json): Either[CodecError, Absence] =
    Wire.field[String](json, "kind").flatMap {
      case "not-recorded" => Right(Absence.NotRecorded)
      case "unparsed"     => Right(Absence.Unparsed)
      case "failed"       =>
        for
          code <- Wire.field[String](json, "code")
          parts = code.split('.')
          valid <- Either.cond(
            parts.length == 2 && parts.forall(_.nonEmpty),
            DiagnosticCode(parts(0), parts(1)),
            CodecError.Field("code", json, "expected family.name")
          )
          message <- Wire.field[String](json, "message")
        yield Absence.Failed(valid, message)
      case "undefined" =>
        Wire.field[Json](json, "reason").flatMap(readUndefined).map(Absence.Undefined(_))
      case "empty-group"   => Right(Absence.EmptyGroup)
      case "unpaired"      => Right(Absence.Unpaired)
      case "below-minimum" =>
        (Wire.field[Int](json, "queries"), Wire.field[Int](json, "required"))
          .mapN(Absence.BelowMinimum(_, _))
      case other => Left(CodecError.Field("kind", json, s"unknown absence '$other'"))
    }

  private def value(v: Value[Double]): Json = v match
    case Value.Present(x) => Json.obj("present" -> Json.fromDoubleOrNull(x))
    case Value.Missing(a) => Json.obj("missing" -> absence(a))

  private def readValue(json: Json): Either[CodecError, Value[Double]] =
    (json.hcursor.downField("present").focus, json.hcursor.downField("missing").focus) match
      case (Some(_), None) =>
        Wire
          .field[Double](json, "present")
          .flatMap(x =>
            Either
              .cond(x.isFinite, Value.Present(x), CodecError.Field("present", json, "finite"))
          )
      case (None, Some(m)) => readAbsence(m).map(Value.Missing(_))
      case _               =>
        Left(CodecError.Field("value", json, "expected exactly one of present or missing"))

  private def valueAt(json: Json, field: String) =
    Wire.field[Json](json, field).flatMap(j => readValue(j).left.map(Wire.at(field)))

  private def spreadJson(d: CellSpread): Json = Json.obj(
    "n"   -> Json.fromInt(d.n),
    "sd"  -> value(d.sd),
    "sem" -> d.sem.fold(Json.Null)(value)
  )

  private def readSpread(json: Json): Either[CodecError, CellSpread] =
    for
      n   <- Wire.field[Int](json, "n")
      sd  <- valueAt(json, "sd")
      sem <- Wire.field[Option[Json]](json, "sem").flatMap(_.traverse(readValue))
    yield CellSpread(n, sd, sem)

  private def group(g: GroupKey): Json = Json.arr(g.levels.map { (t, l) =>
    Json.obj("term" -> Json.fromString(t), "level" -> Json.fromString(l))
  }*)

  private def readGroup(json: Json): Either[CodecError, GroupKey] =
    json.asArray
      .toRight(CodecError.Field("group", json, "expected an array"))
      .flatMap(
        _.toVector
          .traverse(l => (Wire.field[String](l, "term"), Wire.field[String](l, "level")).tupled)
      )
      .map(GroupKey(_))

  private def groupAt(json: Json, field: String) =
    Wire.field[Json](json, field).flatMap(j => readGroup(j).left.map(Wire.at(field)))

  private def participant(p: ParticipantValue): Json = Json.obj(
    "participant" -> Json.fromString(p.participant),
    "queries"     -> Json.fromInt(p.queries),
    "value"       -> value(p.value)
  )

  private def readParticipant(json: Json): Either[CodecError, ParticipantValue] =
    (
      Wire.field[String](json, "participant"),
      Wire.field[Int](json, "queries"),
      valueAt(json, "value")
    )
      .mapN(ParticipantValue(_, _, _))

  private def list[A](json: Json, field: String)(read: Json => Either[CodecError, A]) =
    Wire
      .field[Vector[Json]](json, field)
      .flatMap(_.zipWithIndex.traverse((j, i) => read(j).left.map(Wire.at(s"$field[$i]"))))

  // ------------------------------------------------------------------ report

  /** `eyes4s.report@1` over keys written by `keys`: the specification, the
    * binding, and every group, cell, level contrast, accounting and
    * finding. Cell members are written as keys; each member's reference is
    * the cell role's row of that key at the specification's scale.
    */
  def report[K](keys: VersionedCodec[K]): VersionedCodec[Report[K]] =
    def key(k: K)                          = keys.encode(k)
    def readKey(json: Json, field: String) =
      Wire.field[Json](json, field).flatMap(j => keys.decode(j).left.map(Wire.at(field)))
    def memberKey(ref: ResultRef[K]): Either[CodecError, Json] = ref match
      case ResultRef.Reduction(_, _, k) => key(k)
      case ResultRef.ContrastRow(_, k)  => key(k)
      case other                        =>
        Left(CodecError.Unsupported("members", s"a report cell cannot cite $other"))
    def ref(role: Role, scale: Int, k: K): ResultRef[K] = role match
      case Role.Matched    => ResultRef.Reduction(scale, StudyDesign.Matched, k)
      case Role.Control    => ResultRef.Reduction(scale, StudyDesign.Control, k)
      case Role.Difference => ResultRef.ContrastRow(scale, k)

    def cell(c: Cell[K]): Either[CodecError, Json] =
      c.members.traverse(memberKey).map { members =>
        Json.obj(
          "group"          -> group(c.group),
          "role"           -> Json.fromString(role(c.role)),
          "component"      -> Json.fromString(c.component),
          "estimate"       -> value(c.estimate),
          "spread"         -> spreadJson(c.dispersion),
          "participants"   -> Json.fromInt(c.participants),
          "queries"        -> Json.fromInt(c.queries),
          "perParticipant" -> Json.arr(c.perParticipant.map(participant)*),
          "members"        -> Json.arr(members*)
        )
      }
    def readCell(scale: Int)(json: Json): Either[CodecError, Cell[K]] =
      for
        g         <- groupAt(json, "group")
        r         <- readRole(json, "role")
        component <- Wire.field[String](json, "component")
        estimate  <- valueAt(json, "estimate")
        spread    <- Wire.field[Json](json, "spread").flatMap(readSpread)
        people    <- Wire.field[Int](json, "participants")
        queries   <- Wire.field[Int](json, "queries")
        per       <- list(json, "perParticipant")(readParticipant)
        members   <- list(json, "members")(keys.decode)
      yield Cell(
        g,
        r,
        component,
        estimate,
        spread,
        people,
        queries,
        per,
        members.map(ref(r, scale, _))
      )

    def stat(s: LevelContrastStat): Json = Json.obj(
      "stratum"    -> group(s.stratum),
      "term"       -> Json.fromString(s.term),
      "minuend"    -> Json.fromString(s.minuend),
      "subtrahend" -> Json.fromString(s.subtrahend),
      "role"       -> Json.fromString(role(s.role)),
      "component"  -> Json.fromString(s.component),
      "estimate"   -> value(s.estimate),
      "spread"     -> spreadJson(s.dispersion),
      "paired"     -> Json.arr(s.paired.map(participant)*),
      "unpaired"   -> Json.arr(s.unpaired.map { u =>
        Json.obj(
          "participant" -> Json.fromString(u.participant),
          "present"     -> Json.fromString(u.present),
          "missing"     -> Json.fromString(u.missing)
        )
      }*)
    )
    def readStat(json: Json): Either[CodecError, LevelContrastStat] =
      for
        stratum    <- groupAt(json, "stratum")
        t          <- Wire.field[String](json, "term")
        minuend    <- Wire.field[String](json, "minuend")
        subtrahend <- Wire.field[String](json, "subtrahend")
        r          <- readRole(json, "role")
        component  <- Wire.field[String](json, "component")
        estimate   <- valueAt(json, "estimate")
        spread     <- Wire.field[Json](json, "spread").flatMap(readSpread)
        paired     <- list(json, "paired")(readParticipant)
        unpaired   <- list(json, "unpaired")(u =>
          (
            Wire.field[String](u, "participant"),
            Wire.field[String](u, "present"),
            Wire.field[String](u, "missing")
          ).mapN(UnpairedParticipant(_, _, _))
        )
      yield LevelContrastStat(
        stratum,
        t,
        minuend,
        subtrahend,
        r,
        component,
        estimate,
        spread,
        paired,
        unpaired
      )

    def books(a: Accounting): Json = Json.obj(
      "role"                  -> Json.fromString(role(a.role)),
      "eligible"              -> Json.fromInt(a.eligible),
      "kept"                  -> Json.fromInt(a.kept),
      "filteredOut"           -> Json.fromInt(a.filteredOut),
      "unknownPredicate"      -> Json.fromInt(a.unknownPredicate),
      "failed"                -> Json.fromInt(a.failed),
      "missingGroupAttribute" -> Json.fromInt(a.missingGroupAttribute),
      "belowMinimum"          -> Json.fromInt(a.belowMinimum)
    )
    def readBooks(json: Json): Either[CodecError, Accounting] =
      for
        r        <- readRole(json, "role")
        eligible <- Wire.field[Int](json, "eligible")
        kept     <- Wire.field[Int](json, "kept")
        filtered <- Wire.field[Int](json, "filteredOut")
        unknown  <- Wire.field[Int](json, "unknownPredicate")
        failed   <- Wire.field[Int](json, "failed")
        missing  <- Wire.field[Int](json, "missingGroupAttribute")
        below    <- Wire.field[Int](json, "belowMinimum")
        made     <- Accounting
          .of(r, eligible, kept, filtered, unknown, failed, missing, below)
          .left
          .map(CodecError.Report.apply)
      yield made

    def finding(f: ReportFinding[K]): Either[CodecError, Json] =
      def kind(k: String) = "kind" -> Json.fromString(k)
      f match
        case ReportFinding.EmptyGroup(g, r) =>
          Right(
            Json.obj(
              kind("empty-group"),
              "group" -> group(g),
              "role"  -> Json.fromString(role(r))
            )
          )
        case ReportFinding.UnpairedParticipant(p, s, r, missing) =>
          Right(
            Json.obj(
              kind("unpaired-participant"),
              "participant" -> Json.fromString(p),
              "stratum"     -> group(s),
              "role"        -> Json.fromString(role(r)),
              "missing"     -> Json.fromString(missing)
            )
          )
        case ReportFinding.MissingCovariate(k, t) =>
          key(k).map(j =>
            Json.obj(kind("missing-covariate"), "key" -> j, "term" -> Json.fromString(t))
          )
        case ReportFinding.UnknownPredicate(k, t) =>
          key(k).map(j =>
            Json.obj(kind("unknown-predicate"), "key" -> j, "term" -> Json.fromString(t))
          )
        case ReportFinding.BelowMinimum(p, g, r, queries, required) =>
          Right(
            Json.obj(
              kind("below-minimum"),
              "participant" -> Json.fromString(p),
              "group"       -> group(g),
              "role"        -> Json.fromString(role(r)),
              "queries"     -> Json.fromInt(queries),
              "required"    -> Json.fromInt(required)
            )
          )
        case ReportFinding.UndefinedWindowShare(k, m) =>
          key(k).map(j =>
            Json.obj(
              kind("undefined-window-share"),
              "key"     -> j,
              "measure" -> Json.fromString(measure(m))
            )
          )
        case ReportFinding.CovariateType(k, c, raw, expected) =>
          key(k).map(j =>
            Json.obj(
              kind("covariate-type"),
              "key"       -> j,
              "covariate" -> Json.fromString(c),
              "raw"       -> Json.fromString(raw),
              "expected"  -> Json.fromString(expected)
            )
          )
    def readFinding(json: Json): Either[CodecError, ReportFinding[K]] =
      Wire.field[String](json, "kind").flatMap {
        case "empty-group" =>
          (groupAt(json, "group"), readRole(json, "role")).mapN(ReportFinding.EmptyGroup(_, _))
        case "unpaired-participant" =>
          (
            Wire.field[String](json, "participant"),
            groupAt(json, "stratum"),
            readRole(json, "role"),
            Wire.field[String](json, "missing")
          ).mapN(ReportFinding.UnpairedParticipant(_, _, _, _))
        case "missing-covariate" =>
          (readKey(json, "key"), Wire.field[String](json, "term"))
            .mapN(ReportFinding.MissingCovariate(_, _))
        case "unknown-predicate" =>
          (readKey(json, "key"), Wire.field[String](json, "term"))
            .mapN(ReportFinding.UnknownPredicate(_, _))
        case "below-minimum" =>
          (
            Wire.field[String](json, "participant"),
            groupAt(json, "group"),
            readRole(json, "role"),
            Wire.field[Int](json, "queries"),
            Wire.field[Int](json, "required")
          ).mapN(ReportFinding.BelowMinimum(_, _, _, _, _))
        case "undefined-window-share" =>
          (readKey(json, "key"), enumField(json, "measure", WindowMeasure.values)(measure))
            .mapN(ReportFinding.UndefinedWindowShare(_, _))
        case "covariate-type" =>
          (
            readKey(json, "key"),
            Wire.field[String](json, "covariate"),
            Wire.field[String](json, "raw"),
            Wire.field[String](json, "expected")
          ).mapN(ReportFinding.CovariateType(_, _, _, _))
        case other => Left(CodecError.Field("kind", json, s"unknown finding '$other'"))
      }

    def bindingJson(b: ReportBinding): Json = Json.obj(
      "plan"       -> Json.fromString(b.plan.hex),
      "input"      -> Json.fromString(b.input.hex),
      "result"     -> Json.fromString(b.result.hex),
      "covariates" -> b.covariates.fold(Json.Null)(d => Json.fromString(d.hex))
    )
    def readBinding(json: Json): Either[CodecError, ReportBinding] =
      def at(field: String) =
        Wire
          .field[String](json, field)
          .flatMap(h => BindingDigest.parse(field, h).left.map(CodecError.Report.apply))
      for
        p <- at("plan")
        i <- at("input")
        r <- at("result")
        c <- Wire
          .field[Option[String]](json, "covariates")
          .flatMap(
            _.traverse(h =>
              BindingDigest.parse("covariates", h).left.map(CodecError.Report.apply)
            )
          )
      yield ReportBinding(p, i, r, c)

    VersionedCodec.checked[Report[K]](D.report) { r =>
      for
        s        <- reportSpec.encode(r.spec)
        cells    <- r.cells.traverse(cell)
        findings <- r.findings.traverse(finding)
      yield Json.obj(
        "spec"       -> s,
        "binding"    -> bindingJson(r.binding),
        "groups"     -> Json.arr(r.groups.map(group)*),
        "cells"      -> Json.arr(cells*),
        "contrasts"  -> Json.arr(r.contrasts.map(stat)*),
        "accounting" -> Json.arr(r.accounting.map(books)*),
        "findings"   -> Json.arr(findings*)
      )
    } { json =>
      for
        s <- Wire
          .field[Json](json, "spec")
          .flatMap(j => reportSpec.decode(j).left.map(Wire.at("spec")))
        b <- Wire
          .field[Json](json, "binding")
          .flatMap(j => readBinding(j).left.map(Wire.at("binding")))
        groups     <- list(json, "groups")(readGroup)
        cells      <- list(json, "cells")(readCell(s.scale))
        contrasts  <- list(json, "contrasts")(readStat)
        accounting <- list(json, "accounting")(readBooks)
        findings   <- list(json, "findings")(readFinding)
        made       <- Report
          .reconstruct(s, b, groups, cells, contrasts, accounting, findings)
          .left
          .map(CodecError.Report.apply)
      yield made
    }
