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

import cats.data.NonEmptyVector
import eyes4s.design.*
import eyes4s.kernel.*

/** Checks that a projection keeps every field of its case under the field's
  * name and with the field's value, recursively through nested errors, and
  * that every error an error wraps belongs to a sampled, cataloged family.
  * Shared by the plan and codec catalog suites; `structured` and `plain`
  * extend it with a module's own structured field types.
  */
final class DiagnosticAlignment(
    families: Vector[FamilySamples],
    structuredExtra: PartialFunction[Any, Operand[Any]] = PartialFunction.empty,
    plainExtra: PartialFunction[(Any, Operand[Any]), Boolean] = PartialFunction.empty
):
  /** The fully qualified enum a case belongs to, from its runtime class. */
  def enumOf(value: scala.reflect.Enum): String =
    value.getClass.getName.takeWhile(_ != '$')

  /** An enum of errors, refusals, failures or findings, which a projection
    * must carry as a cataloged cause and never as a token.
    */
  def errorLike(value: scala.reflect.Enum): Boolean =
    val name = enumOf(value).split('.').last
    Vector("Error", "Failure", "Mismatch", "Cause", "Reason", "Finding").exists(name.endsWith)

  /** The sampled family that owns a case, found by its runtime class. */
  def familyOf(raw: scala.reflect.Enum): Option[DiagnosticFamily] =
    families.find(_.samples.exists(_._1.getClass == raw.getClass)).map(_.family)

  /** Error-like values a sample wraps whose family is not sampled. */
  def uncataloged(raw: scala.reflect.Enum): Vector[String] =
    def within(value: Any): Vector[String] = value match
      case e: scala.reflect.Enum if errorLike(e) =>
        (if familyOf(e).isEmpty then Vector(enumOf(e)) else Vector.empty) ++
          e.productIterator.toVector.flatMap(within)
      case p: Product if !p.isInstanceOf[scala.reflect.Enum] =>
        p.productIterator.toVector.flatMap(within)
      case v: Iterable[?]        => v.toVector.flatMap(within)
      case v: NonEmptyVector[?]  => v.toVector.flatMap(within)
      case e: scala.reflect.Enum => e.productIterator.toVector.flatMap(within)
      case _                     => Vector.empty
    raw.productIterator.toVector.flatMap(within).distinct

  /** Problems with a projection: operand names must be the case's field
    * names, and each value must carry the field it names.
    */
  def aligned(raw: scala.reflect.Enum, d: Diagnostic[Any]): Vector[String] =
    val names = raw.productElementNames.toVector
    if d.operands.map(_._1) != names then
      Vector(s"$raw operands ${d.operands.map(_._1)} are not its fields $names")
    else
      raw.productIterator.toVector.zip(d.operands).flatMap { case (value, (name, operand)) =>
        if matches(value, operand) then Vector.empty
        else Vector(s"$raw field $name=$value projects to $operand")
      }

  private def nestedCode(raw: scala.reflect.Enum, d: Diagnostic[Any]): Boolean =
    familyOf(raw).exists(family => family.codes.lift(raw.ordinal).contains(d.code)) &&
      aligned(raw, d).isEmpty

  /** The structured projection a known structured field must have. */
  private def structured(value: Any): Option[Operand[Any]] =
    import DiagnosticSupport.*
    structuredExtra
      .lift(value)
      .orElse(value match
        case SourceInterpretation.LegacyUnspecified => Some(Operand.Token("LegacyUnspecified"))
        case v: SourceInterpretation.Declared       =>
          Some(
            Operand.Fields(
              Vector(
                "kind"          -> Operand.Token("Declared"),
                "format"        -> Operand.Token(v.format.toString),
                "parser"        -> Operand.Definition(v.parser),
                "optionsSchema" -> Operand.Token(v.optionsSchema.toString),
                "options"       -> Operand.Artifact(v.options.render)
              )
            )
          )
        case v: Interval                    => Some(interval(v))
        case v: eyes4s.core.SampleRange     => Some(range(v))
        case v: FrameSpec                   => Some(frameSpec(v))
        case v: GridSpec                    => Some(gridSpec(v))
        case v: eyes4s.detect.DetectorRef   => Some(detector(v))
        case v: EvaluationSpec              => Some(specification(v))
        case v: EvaluationGeometry          => Some(evaluationGeometry(v))
        case v: EvaluationInfo              => Some(evaluation(v))
        case v: EvaluationTime              => Some(evaluationTime(v))
        case v: eyes4s.compare.MeasureScale => Some(measureScale(v))
        case v: EvaluationScale             => Some(evaluationScale(v))
        case v: eyes4s.detect.GapPolicy     => Some(gapPolicy(v))
        case v: LedgerRefusal[?]            => Some(Operand.Cause(Projections.ledgerRefusal(v)))
        case v: WindowTally                 => Some(windowTally(v))
        case _                              => None)

  def matches(value: Any, operand: Operand[Any]): Boolean =
    structured(value) match
      case Some(expected) => operand == expected
      case None           => plainExtra.lift((value, operand)).getOrElse(plain(value, operand))

  private def plain(value: Any, operand: Operand[Any]): Boolean = (value, operand) match
    case (v: Int, Operand.Integer(n))                   => n == BigInt(v)
    case (v: Long, Operand.Integer(n))                  => n == BigInt(v)
    case (v: Long, Operand.Micros(n))                   => n == v
    case (v: BigInt, Operand.Integer(n))                => n == v
    case (v: Double, Operand.Real(x))                   => x == ExactDouble(v)
    case (v: String, Operand.Name(x))                   => x == v
    case (v: String, Operand.Text(x))                   => x == v
    case (v: String, Operand.Token(x))                  => x == v
    case (v: String, Operand.Artifact(x))               => x == v
    case (v: StudyKey, Operand.Key(x))                  => x == v
    case (v: DefinitionId, Operand.Definition(x))       => x == v
    case (v: ArtifactRef[?], Operand.Artifact(x))       => x == v.digest
    case (v: FrameId, Operand.Name(x))                  => x == v.name
    case (v: ClockId, Operand.Name(x))                  => x == v.name
    case (v: GridId, Operand.Name(x))                   => x == v.name
    case (v: eyes4s.core.RecordingRef, Operand.Name(x)) => x == v.value
    case (v: eyes4s.core.RecordingRef, Operand.Text(x)) => x == v.value
    case (v: Provenance, Operand.Lineage(x))            => x == v
    case (v: ResultRef[?], Operand.Fields(fields))      =>
      fields.headOption.contains("kind" -> Operand.Token(v.productPrefix)) &&
      fields.size == v.productArity + 1
    case (v: scala.reflect.Enum, Operand.Cause(d)) => nestedCode(v, d)
    case (v: scala.reflect.Enum, Operand.Token(x)) =>
      !errorLike(v) && (x == v.toString || (v match
        case p: FailurePolicy => x == p.render
        case _                => false))
    case (None, Operand.Absent)        => true
    case (Some(inner), found)          => found != Operand.Absent && matches(inner, found)
    case (Right(()), Operand.Token(_)) => true
    case (Left(e: scala.reflect.Enum), Operand.Cause(d)) => nestedCode(e, d)
    case (v: NonEmptyVector[?], Operand.Causes(ds))      =>
      ds.size == v.length && v.toVector.zip(ds).forall {
        case (e: scala.reflect.Enum, d) => nestedCode(e, d)
        case _                          => false
      }
    case (v: Vector[?], Operand.Causes(ds)) =>
      ds.size == v.size && v.zip(ds).forall {
        case (e: scala.reflect.Enum, d) => nestedCode(e, d)
        case _                          => false
      }
    case (v: Vector[?], Operand.Integers(xs)) =>
      xs.size == v.size && v.zip(xs).forall {
        case (n: Int, x) => x == BigInt(n)
        case _           => false
      }
    case (v: Vector[?], Operand.Names(xs))      => xs == v.map(_.toString)
    case (v: Vector[?], Operand.Keys(xs))       => xs == v
    case (v: Vector[?], Operand.Params(xs))     => xs == v
    case (v: Vector[?], Operand.Parameters(xs)) => xs == v
    case (v: Vector[?], Operand.Items(xs))      =>
      xs.size == v.size && v.zip(xs).forall {
        case (change: PlanChange, Operand.Fields(fields)) =>
          fields == Vector(
            "field"  -> Operand.Name(change.field),
            "before" -> Operand.Params(change.before),
            "after"  -> Operand.Params(change.after)
          )
        case (step: Provenance.Step, Operand.Fields(fields)) =>
          fields == Vector(
            "operation" -> Operand.Name(step.operation),
            "params"    -> Operand.Parameters(step.params)
          )
        case (item, found) => matches(item, found)
      }
    case (v: Set[?], Operand.Names(xs))    => xs == v.toVector.map(_.toString).sorted
    case (v: Byte, Operand.Integer(n))     => n == BigInt(v)
    case (v: Char, Operand.Text(x))        => x == v.toString
    case (v: BigDecimal, Operand.Text(x))  => x == v.toString
    case (v: Boolean, Operand.Token(x))    => x == v.toString
    case (v: Long, Operand.Artifact(x))    => x == f"$v%016x" // a ContentHash
    case (v: PredictorId, Operand.Name(x)) => x == v.value
    case (v: eyes4s.core.TemporalSupport, Operand.Token(x)) => x == v.render
    // Any other sum case with fields: its kind, then its named fields.
    case (v: scala.reflect.Enum, Operand.Fields(("kind", Operand.Token(label)) +: rest)) =>
      !errorLike(v) && label == v.productPrefix && named(v, rest)
    // Any other product: its named fields.
    case (v: Product, Operand.Fields(fields)) if !v.isInstanceOf[scala.reflect.Enum] =>
      named(v, fields)
    case _ => false

  private def named(value: Product, fields: Vector[(String, Operand[Any])]): Boolean =
    fields.map(_._1) == value.productElementNames.toVector &&
      value.productIterator.toVector.zip(fields).forall((v, field) => matches(v, field._2))

  /** Every family's check at once, for a suite: problems as readable lines. */
  def problems: Vector[String] =
    families.flatMap(family =>
      family.samples.flatMap((sample, d) =>
        aligned(sample, d) ++ uncataloged(sample).map(e => s"$sample wraps uncataloged $e")
      )
    )
