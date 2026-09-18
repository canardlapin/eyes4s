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
final case class DiagnosticCode private[plan] (family: String, name: String) derives CanEqual:
  def render: String            = s"$family.$name"
  override def toString: String = render

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

  /** The error kept only a key digest, and no single input trial has it. */
  case UnresolvedDigest(digest: String)

  /** The error named an input position this input does not have. */
  case UnknownInputTrial(index: Int, trials: Int)

  /** The ledger rejected every record of this key, so it has no fixations. */
  case NotAdmitted(key: K, records: Vector[Int])

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

/** A renderer-neutral projection of one typed error or finding.
  *
  * `code` is the stable identity, `subject` names the failing object, and
  * `operands` carry every field of the underlying case under its field name,
  * nested errors included as [[Operand.Cause]]. `sources` hold source links
  * once a [[StudySources]] index has linked the diagnostic. A preflight
  * finding also carries its library-owned category and remedy. `message` is a
  * default English rendering; it is never an identity and may differ between
  * platforms where numbers render differently.
  */
final case class Diagnostic[+K](
    code: DiagnosticCode,
    severity: DiagnosticSeverity,
    subject: Vector[Locus[K]],
    operands: Vector[(String, Operand[K])],
    sources: Vector[SourceLink[K]],
    message: String,
    category: Option[FindingClass] = None,
    remedy: Option[Remedy] = None
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
  def keys: Vector[K] = subject.flatMap {
    case Locus.Trial(key)             => Vector(key)
    case Locus.Trials(keys)           => keys
    case Locus.Pair(focal, reference) => Vector(focal, reference)
    case _                            => Vector.empty
  }

  /** The same diagnostic located inside a coarser context (scale, design, window). */
  def within[K2 >: K](context: Locus[K2]*): Diagnostic[K2] =
    copy(subject = context.toVector ++ subject)

  def withSeverity(value: DiagnosticSeverity): Diagnostic[K] = copy(severity = value)

  def linked[K2 >: K](links: Vector[SourceLink[K2]]): Diagnostic[K2] =
    copy(sources = sources ++ links)

object Diagnostic:
  /** Project any cataloged error through its [[Diagnose]] instance. */
  def of[E, K](error: E)(using diagnose: Diagnose[E, K]): Diagnostic[K] = diagnose(error)

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
  private[plan] def of(name: String, severity: DiagnosticSeverity)(
      labels: String*
  ): DiagnosticFamily = new DiagnosticFamily(name, labels.toVector, severity)

  private[plan] def error(name: String)(labels: String*): DiagnosticFamily =
    of(name, DiagnosticSeverity.Error)(labels*)

  /** `InvalidDefinition` becomes `invalid-definition`. */
  def slug(label: String): String =
    label.zipWithIndex.map { case (c, i) =>
      if c.isUpper && i > 0 then "-" + c.toLower else c.toLower.toString
    }.mkString
