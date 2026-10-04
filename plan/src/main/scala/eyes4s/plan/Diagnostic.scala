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

package eyes4s.plan

import eyes4s.core.RecordingRef
import eyes4s.kernel.Provenance

/** Stable identity of one kind of diagnostic: a family prefix and a case slug,
  * rendered `family.case`. Codes are fixed by [[DiagnosticCatalog]], identical
  * on every platform and run, and never derived from message text. A consumer
  * localises from the code and the named operands.
  */
final case class DiagnosticCode private[eyes4s] (family: String, name: String) derives CanEqual:
  def render: String            = s"$family.$name"
  override def toString: String = render

object DiagnosticCode:
  private val Slug = "[a-z][a-z0-9]*(-[a-z0-9]+)*".r

  /** A code for a host application's own check, which it reports with
    * [[DiagnosticSource.Host]]. Both parts must be lower-case kebab-case
    * slugs, the form every library code takes.
    */
  def host(family: String, name: String): Either[DiagnosticCodeError, DiagnosticCode] =
    for
      _ <- Either.cond(Slug.matches(family), (), DiagnosticCodeError.InvalidFamily(family))
      _ <- Either.cond(Slug.matches(name), (), DiagnosticCodeError.InvalidName(name))
    yield DiagnosticCode(family, name)

/** Why a host's diagnostic code was refused. */
enum DiagnosticCodeError derives CanEqual:
  case InvalidFamily(family: String)
  case InvalidName(name: String)

  def message: String = this match
    case InvalidFamily(family) =>
      s"Diagnostic family '$family' is not a lower-case kebab-case slug."
    case InvalidName(name) => s"Diagnostic name '$name' is not a lower-case kebab-case slug."

/** Who reported a diagnostic. Every diagnostic this library projects is
  * `EyesCore`; a host application reports its own checks as `Host`, so an
  * application can show the two as separate, labelled sources without
  * inspecting codes.
  */
enum DiagnosticSource derives CanEqual:
  case EyesCore, Host

/** An error prevents the operation, or the named item, from producing a value.
  * A warning leaves execution possible but names a deterministic risk or a
  * missing explanation (preflight warnings are the only source).
  */
enum DiagnosticSeverity derives CanEqual:
  case Error, Warning

/** One step of the path from the whole operation to the object that failed.
  * A diagnostic's subject lists loci from the coarsest (scale, design,
  * repetition) to the finest (trial, pair, fixation, record). Positions are
  * named as such: [[Locus.InputTrial]] is an index into a supplied input, not
  * an identity, and [[Locus.TrialDigest]] is a digest the underlying error
  * retained instead of the typed key.
  */
enum Locus[+K] derives CanEqual:
  case Artifact(digest: String)
  case Definition(id: DefinitionId)
  case Field(name: String)
  case Scale(index: Int)
  case Design(design: StudyDesign)
  case Repetition(name: String)
  case Window(name: String)
  case Trial(key: K)
  case Trials(keys: Vector[K])
  case TrialDigest(digest: String)
  case Pair(focal: K, reference: K)
  case Fixation(index: Int)
  case Record(number: Int)
  case Records(numbers: Vector[Int])
  case InputTrial(index: Int)
  case Recording(source: RecordingRef)
  case Area(id: String)
  case Event(index: Int)
  case Sample(index: Int)
  case Samples(from: Int, until: Int)

  /** An entry of a saved-study manifest, by its artifact name. */
  case Entry(name: String)

  /** A location inside a decoded document, as the codec reports it. */
  case Path(path: String)

  /** A typed manifest relation, by its kind and its source entry. */
  case Relation(kind: String, source: String)

  /** A physical line of a named text source, such as an EyeLink ASC file,
    * counted from 1.
    */
  case Line(source: String, line: Long)

  /** A participant of a report, by the name the study layout projects. */
  case Participant(name: String)

  /** A report group: one level of each grouping term, as `(term, level)`,
    * in grouping order; no levels is the whole report.
    */
  case Group(levels: Vector[(String, String)])

  /** Trial keys this locus names. */
  def trialKeys: Vector[K] = this match
    case Trial(key)             => Vector(key)
    case Trials(keys)           => keys
    case Pair(focal, reference) => Vector(focal, reference)
    case _                      => Vector.empty

  /** The same locus with its trial keys transformed. */
  def mapKeys[K2](f: K => K2): Locus[K2] = this match
    case Trial(key)             => Trial(f(key))
    case Trials(keys)           => Trials(keys.map(f))
    case Pair(focal, reference) => Pair(f(focal), f(reference))
    case Artifact(digest)       => Artifact(digest)
    case Definition(id)         => Definition(id)
    case Field(name)            => Field(name)
    case Scale(index)           => Scale(index)
    case Design(design)         => Design(design)
    case Repetition(name)       => Repetition(name)
    case Window(name)           => Window(name)
    case TrialDigest(digest)    => TrialDigest(digest)
    case Fixation(index)        => Fixation(index)
    case Record(number)         => Record(number)
    case Records(numbers)       => Records(numbers)
    case InputTrial(index)      => InputTrial(index)
    case Recording(source)      => Recording(source)
    case Area(id)               => Area(id)
    case Event(index)           => Event(index)
    case Sample(index)          => Sample(index)
    case Samples(from, until)   => Samples(from, until)
    case Entry(name)            => Entry(name)
    case Path(path)             => Path(path)
    case Relation(kind, source) => Relation(kind, source)
    case Line(source, line)     => Line(source, line)
    case Participant(name)      => Participant(name)
    case Group(levels)          => Group(levels)

/** A double compared by its bit pattern: NaN equals NaN and `-0.0` differs
  * from `0.0`, so a projection of a non-finite failure equals itself and exact
  * values are never compared approximately.
  */
final class ExactDouble private (val value: Double):
  override def equals(other: Any): Boolean = other match
    case that: ExactDouble =>
      java.lang.Double.doubleToLongBits(value) == java.lang.Double.doubleToLongBits(that.value)
    case _ => false
  override def hashCode: Int    = java.lang.Double.doubleToLongBits(value).hashCode
  override def toString: String = value.toString

object ExactDouble:
  def apply(value: Double): ExactDouble         = new ExactDouble(value)
  def unapply(exact: ExactDouble): Some[Double] = Some(exact.value)
  given CanEqual[ExactDouble, ExactDouble]      = CanEqual.derived

/** A typed operand of a diagnostic. Operand names are the field names of the
  * error case they came from, so `code` plus the named operands identify the
  * diagnostic completely; [[Operand.Text]] marks free text produced outside
  * the catalog (a key reader's reason, a rendered span), which a consumer can
  * show verbatim but not localise.
  */
enum Operand[+K] derives CanEqual:
  case Key(value: K)
  case Keys(values: Vector[K])
  case Integer(value: BigInt)
  case Integers(values: Vector[BigInt])

  /** A measured or declared value at full precision, compared exactly. */
  case Real(value: ExactDouble)

  /** Signed 64-bit microseconds: an instant on its clock, or a span. */
  case Micros(value: Long)

  /** A stable library vocabulary token: an enum case or a documented rendering. */
  case Token(value: String)

  /** A name supplied by a plan or its data: a column, window, area or identity. */
  case Name(value: String)
  case Names(values: Vector[String])
  case Text(value: String)
  case Definition(id: DefinitionId)
  case Artifact(digest: String)
  case Parameters(values: Vector[(String, Provenance.Param)])
  case Params(values: Vector[Provenance.Param])
  case Lineage(value: Provenance)

  /** A structured value, decomposed into named operands. */
  case Fields(values: Vector[(String, Operand[K])])
  case Items(values: Vector[Operand[K]])

  /** A nested typed error, projected through its own family. */
  case Cause(diagnostic: Diagnostic[K])
  case Causes(diagnostics: Vector[Diagnostic[K]])

  /** An optional operand that is absent; distinct from zero or empty. */
  case Absent

  /** Trial keys this operand carries, nested causes included, in order. */
  def trialKeys: Vector[K] = this match
    case Key(value)          => Vector(value)
    case Keys(values)        => values
    case Fields(values)      => values.flatMap(_._2.trialKeys)
    case Items(values)       => values.flatMap(_.trialKeys)
    case Cause(diagnostic)   => diagnostic.affectedTrials
    case Causes(diagnostics) => diagnostics.flatMap(_.affectedTrials)
    case _                   => Vector.empty

  /** Every key, source links of nested causes included. */
  private[plan] def everyKey: Vector[K] = this match
    case Fields(values)      => values.flatMap(_._2.everyKey)
    case Items(values)       => values.flatMap(_.everyKey)
    case Cause(diagnostic)   => diagnostic.everyKey
    case Causes(diagnostics) => diagnostics.flatMap(_.everyKey)
    case other               => other.trialKeys

  /** The same operand with its trial keys transformed. */
  def mapKeys[K2](f: K => K2): Operand[K2] = this match
    case Key(value)          => Key(f(value))
    case Keys(values)        => Keys(values.map(f))
    case Fields(values)      => Fields(values.map((name, value) => name -> value.mapKeys(f)))
    case Items(values)       => Items(values.map(_.mapKeys(f)))
    case Cause(diagnostic)   => Cause(diagnostic.mapKeys(f))
    case Causes(diagnostics) => Causes(diagnostics.map(_.mapKeys(f)))
    case Integer(value)      => Integer(value)
    case Integers(values)    => Integers(values)
    case Real(value)         => Real(value)
    case Micros(value)       => Micros(value)
    case Token(value)        => Token(value)
    case Name(value)         => Name(value)
    case Names(values)       => Names(values)
    case Text(value)         => Text(value)
    case Definition(id)      => Definition(id)
    case Artifact(digest)    => Artifact(digest)
    case Parameters(values)  => Parameters(values)
    case Params(values)      => Params(values)
    case Lineage(value)      => Lineage(value)
    case Absent              => Absent

/** Why no source evidence could be named for a subject. */
enum MissingSource[+K] derives CanEqual:
  /** The input was not linked to an admission ledger. */
  case NoLedger

  /** The ledger names no record for this key. */
  case UnknownTrial(key: K)

  /** The trial has fewer fixations than the index asked for. */
  case FixationOutOfRange(key: K, index: Int, fixations: Int)

  /** The trial's scanpath carries no recording sample support. */
  case NotSourceSupported(key: K)

  /** The input repeats this full key, so no single trial can be addressed. */
  case AmbiguousTrial(key: K, occurrences: Int)

  /** The error kept only a key digest, and no input trial has it. */
  case UnknownDigest(digest: String)

  /** The error kept only a key digest, and several input trials have it. */
  case CollidingDigest(digest: String, keys: Vector[K])

  /** The error named an input position this input does not have. */
  case UnknownInputTrial(index: Int, trials: Int)

  /** The ledger rejected every record of this key, so it has no fixations. */
  case NotAdmitted(key: K, records: Vector[Int])

  /** The reason in words, naming its operands. */
  def message: String = this match
    case NoLedger          => "the input is linked to no admission ledger"
    case UnknownTrial(key) => s"the ledger names no record of trial $key"
    case FixationOutOfRange(key, index, fixations) =>
      s"trial $key has $fixations fixations, so none at index $index"
    case NotSourceSupported(key) => s"trial $key's scanpath carries no recording sample support"
    case AmbiguousTrial(key, n)  => s"the input repeats trial $key $n times"
    case UnknownDigest(digest)   => s"no input trial has key digest $digest"
    case CollidingDigest(digest, ks) => s"trials ${ks.mkString(", ")} share key digest $digest"
    case UnknownInputTrial(index, trials) =>
      s"the input has $trials trials, none at position $index"
    case NotAdmitted(key, records) =>
      s"the ledger rejected every record of trial $key (records ${records.mkString(", ")})"

  /** Trial keys this reason names. */
  def trialKeys: Vector[K] = this match
    case UnknownTrial(key)             => Vector(key)
    case FixationOutOfRange(key, _, _) => Vector(key)
    case NotSourceSupported(key)       => Vector(key)
    case AmbiguousTrial(key, _)        => Vector(key)
    case CollidingDigest(_, keys)      => keys
    case NotAdmitted(key, _)           => Vector(key)
    case NoLedger                      => Vector.empty
    case UnknownDigest(_)              => Vector.empty
    case UnknownInputTrial(_, _)       => Vector.empty

  /** The same reason with its trial keys transformed. */
  def mapKeys[K2](f: K => K2): MissingSource[K2] = this match
    case NoLedger                             => NoLedger
    case UnknownTrial(key)                    => UnknownTrial(f(key))
    case FixationOutOfRange(key, index, size) => FixationOutOfRange(f(key), index, size)
    case NotSourceSupported(key)              => NotSourceSupported(f(key))
    case AmbiguousTrial(key, occurrences)     => AmbiguousTrial(f(key), occurrences)
    case UnknownDigest(digest)                => UnknownDigest(digest)
    case CollidingDigest(digest, keys)        => CollidingDigest(digest, keys.map(f))
    case UnknownInputTrial(index, trials)     => UnknownInputTrial(index, trials)
    case NotAdmitted(key, records)            => NotAdmitted(f(key), records)

/** Where the scientific evidence for a subject lives. */
enum SourceLink[+K] derives CanEqual:
  /** A logical record of a ledgered source: the header is record 1. The
    * artifact identity is `source.records`; the label is for display only.
    */
  case Record(source: SourceRef, record: Int)

  /** A fixation by its zero-based position in its trial's scanpath. */
  case Fixation(key: K, index: Int)

  /** Half-open samples `[from, until)` of a recording: its nominal source name
    * and its content identity.
    */
  case Samples(recording: RecordingRef, artifact: ArtifactRef[?], from: Int, until: Int)

  /** An explicit missing locator; never an empty or guessed link. */
  case Missing(reason: MissingSource[K])

  /** Trial keys this link names. */
  def trialKeys: Vector[K] = this match
    case Fixation(key, _) => Vector(key)
    case Missing(reason)  => reason.trialKeys
    case _                => Vector.empty

  /** The same link with its trial keys transformed. */
  def mapKeys[K2](f: K => K2): SourceLink[K2] = this match
    case Record(source, record)                    => Record(source, record)
    case Fixation(key, index)                      => Fixation(f(key), index)
    case Samples(recording, artifact, from, until) => Samples(recording, artifact, from, until)
    case Missing(reason)                           => Missing(reason.mapKeys(f))

/** A renderer-neutral projection of one typed error or finding.
  *
  * `code` is the stable identity, `subject` names the failing object, and
  * `operands` carry every field of the underlying case under its field name,
  * nested errors included as [[Operand.Cause]]. `sources` hold source links
  * once a [[StudySources]] index has linked the diagnostic. A preflight
  * finding also carries its library-owned category and remedy. `source` says
  * who reported it: this library or a host application's own check.
  * `message` is a default English rendering; it is never an identity and may
  * differ between platforms where numbers render differently.
  */
final case class Diagnostic[+K](
    code: DiagnosticCode,
    severity: DiagnosticSeverity,
    subject: Vector[Locus[K]],
    operands: Vector[(String, Operand[K])],
    sources: Vector[SourceLink[K]],
    message: String,
    category: Option[FindingClass] = None,
    remedy: Option[Remedy] = None,
    source: DiagnosticSource = DiagnosticSource.EyesCore
) derives CanEqual:
  def operand(name: String): Option[Operand[K]] = operands.collectFirst {
    case (n, value) if n == name => value
  }

  /** Nested diagnostics, in operand order. */
  def causes: Vector[Diagnostic[K]] = operands.flatMap {
    case (_, Operand.Cause(d))   => Vector(d)
    case (_, Operand.Causes(ds)) => ds
    case _                       => Vector.empty
  }

  /** Trial keys this diagnostic's subject names, in subject order. */
  def keys: Vector[K] = subject.flatMap(_.trialKeys)

  /** Every trial key the diagnostic names, as typed keys: its subject, then
    * its operands and nested causes, each key once in first-seen order. An
    * application opens exactly these trials to act on the diagnostic.
    */
  def affectedTrials: Vector[K] = (keys ++ operands.flatMap(_._2.trialKeys)).distinct

  /** The same diagnostic with every trial key transformed: subject, operands,
    * nested causes and source links.
    */
  def mapKeys[K2](f: K => K2): Diagnostic[K2] =
    Diagnostic(
      code,
      severity,
      subject.map(_.mapKeys(f)),
      operands.map((name, value) => name -> value.mapKeys(f)),
      sources.map(_.mapKeys(f)),
      message,
      category,
      remedy,
      source
    )

  /** Every key anywhere in the diagnostic, source links included. */
  private[plan] def everyKey: Vector[K] =
    keys ++ operands.flatMap(_._2.everyKey) ++ sources.flatMap(_.trialKeys)

  /** The same diagnostic located inside a coarser context (scale, design, window). */
  def within[K2 >: K](context: Locus[K2]*): Diagnostic[K2] =
    copy(subject = context.toVector ++ subject)

  def withSeverity(value: DiagnosticSeverity): Diagnostic[K] = copy(severity = value)

  def linked[K2 >: K](links: Vector[SourceLink[K2]]): Diagnostic[K2] =
    copy(sources = sources ++ links)

object Diagnostic:
  /** Project any cataloged error through its [[Diagnose]] instance. */
  def of[E, K](error: E)(using diagnose: Diagnose[E, K]): Diagnostic[K] = diagnose(error)

  /** A host application's own check, reported with [[DiagnosticSource.Host]]
    * and a code from `DiagnosticCode.host`, so it is never mistaken for a
    * library diagnostic.
    */
  def host[K](
      code: DiagnosticCode,
      severity: DiagnosticSeverity,
      subject: Vector[Locus[K]],
      operands: Vector[(String, Operand[K])],
      message: String
  ): Diagnostic[K] =
    Diagnostic(
      code,
      severity,
      subject,
      operands,
      Vector.empty,
      message,
      source = DiagnosticSource.Host
    )

/** A trial key whose static type the reporting operation cannot state: a key
  * inside an error the codec raised while decoding a saved study of some key
  * schema. `narrow` recovers the typed key when the application knows it.
  */
final class ErasedKey private[eyes4s] (val value: Any):
  /** The key, when it has the type the application expects. `K` must be a
    * class type such as a case class: a type test cannot tell the arguments
    * of a generic type apart, and on Scala.js a whole-number `Double` passes
    * a test for `Int`.
    */
  def narrow[K](using test: scala.reflect.TypeTest[Any, K]): Option[K] = test.unapply(value)

  override def equals(other: Any): Boolean = other match
    case that: ErasedKey => value == that.value
    case _               => false
  override def hashCode: Int    = value.##
  override def toString: String = value.toString

object ErasedKey:
  given CanEqual[ErasedKey, ErasedKey] = CanEqual.derived

  extension (diagnostic: Diagnostic[ErasedKey])
    /** The diagnostic with typed keys, or `None` when any key it carries,
      * source links included, is not a `K`: the error then came from a study
      * of another key schema.
      */
    def narrow[K](using test: scala.reflect.TypeTest[Any, K]): Option[Diagnostic[K]] =
      val erased = diagnostic.everyKey.distinct
      val typed  = erased.flatMap(key => key.narrow[K].map(key -> _)).toMap
      Option.when(typed.size == erased.size)(diagnostic.mapKeys(typed))

/** One enum of typed errors or findings and its code table. `labels` are the
  * enum's case names in declaration order; the code of a case is the family
  * name and the kebab-case label. Tests compare `labels` with the compiler's
  * own list, so a new, removed, renamed or reordered case fails until this
  * table is updated.
  */
final class DiagnosticFamily private (
    val name: String,
    val labels: Vector[String],
    val severity: DiagnosticSeverity
):
  val codes: Vector[DiagnosticCode] =
    labels.map(label => DiagnosticCode(name, DiagnosticFamily.slug(label)))

  /** The code of the case with this enum ordinal. */
  def code(ordinal: Int): DiagnosticCode =
    codes.lift(ordinal).getOrElse(DiagnosticCode(name, s"uncatalogued-$ordinal"))

  def label(ordinal: Int): Option[String] = labels.lift(ordinal)

  override def toString: String = s"DiagnosticFamily($name)"

object DiagnosticFamily:
  private[eyes4s] def of(name: String, severity: DiagnosticSeverity)(
      labels: String*
  ): DiagnosticFamily = new DiagnosticFamily(name, labels.toVector, severity)

  private[eyes4s] def error(name: String)(labels: String*): DiagnosticFamily =
    of(name, DiagnosticSeverity.Error)(labels*)

  /** `InvalidDefinition` becomes `invalid-definition`. */
  def slug(label: String): String =
    label.zipWithIndex.map { case (c, i) =>
      if c.isUpper && i > 0 then "-" + c.toLower else c.toLower.toString
    }.mkString
