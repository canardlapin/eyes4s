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

package eyes4s.studio.core.diff

import eyes4s.studio.core.document.*

/** The reference English rendering of a [[DatasetDiff]] in the board
  * grammar (Data.dc.html history: "onset declared ms; Block → occurrence;
  * 4 trials change status"; Figures.dc.html stale reason: "dataset r3
  * changed the admission status of 4 trials"). Like `FreshnessText`, it is
  * the specification studio-app's strings reproduce, never an identity.
  */
object DatasetDiffText:
  import DatasetChange.*

  def status(s: TrialStatus): String = s match
    case TrialStatus.Admitted          => "admitted"
    case TrialStatus.Quarantined(code) => code.stripPrefix("quarantine.")
    case TrialStatus.NoFixations       => "no-fixations"
    case TrialStatus.Absent            => "absent"
    case TrialStatus.Unlisted          => "not listed"

  private def kind(k: AttributeKindChoice): String = k match
    case AttributeKindChoice.Text    => "text"
    case AttributeKindChoice.Integer => "integer"
    case AttributeKindChoice.Number  => "number"

  private def offScreen(c: OffScreenChoice): String = c match
    case OffScreenChoice.ExcludeRecord   => "exclude record"
    case OffScreenChoice.QuarantineTrial => "quarantine trial"

  private def number(d: Double): String =
    if d == math.rint(d) && math.abs(d) < 1e15 then d.toLong.toString else d.toString

  def rule(r: CorrectionRule): String =
    val what = r.correction match
      case CoordinateCorrection.FlipX        => "flip horizontally"
      case CoordinateCorrection.FlipY        => "flip vertically"
      case CoordinateCorrection.Translate(o) => s"shift (${number(o.dx)}, ${number(o.dy)}) px"
    val target = r.target match
      case CorrectionTarget.AllTrials      => "all trials"
      case CorrectionTarget.Participant(p) => p.value
      case CorrectionTarget.Trial(k)       => k.label
    s"$what for $target"

  /** One change as a phrase; inventory phrases name the inventory. */
  def change(c: DatasetChange): String = c match
    case SourceBytes(role, None, Some(_)) => s"${role.label} source added"
    case SourceBytes(role, Some(_), None) => s"${role.label} source removed"
    case SourceBytes(role, _, _)          => s"${role.label} source replaced"
    case SourceRenamed(role, from, to)    =>
      s"${role.label} source renamed ${from.value} → ${to.value}"
    case SourceIdentified(role, _, Some(_)) => s"${role.label} source identity bound"
    case SourceIdentified(role, _, None)    => s"${role.label} source identity unbound"
    case Mapped(source, role, from, to)     =>
      val phrase = (from, to) match
        case (None, Some(c))    => s"${c.value} → ${role.label}"
        case (Some(c), None)    => s"${role.label} unmapped (was ${c.value})"
        case (Some(a), Some(b)) => s"${role.label}: ${a.value} → ${b.value}"
        case (None, None)       => s"${role.label} unchanged"
      inSource(source, phrase)
    case Attribute(source, column, from, to) =>
      val phrase = (from, to) match
        case (None, Some(k))    => s"${column.value} declared ${kind(k)}"
        case (Some(_), None)    => s"${column.value} no longer declared"
        case (Some(a), Some(b)) => s"${column.value} ${kind(a)} → ${kind(b)}"
        case (None, None)       => s"${column.value} unchanged"
      inSource(source, phrase)
    case InventoryMapped(_, true) => "trial inventory mapped"
    case InventoryMapped(_, _)    => "trial inventory no longer mapped"
    case Units(DeclaredUnits(None), DeclaredUnits(Some(u))) => s"onset declared ${u.symbol}"
    case Units(DeclaredUnits(Some(_)), DeclaredUnits(None)) => "onset units undeclared"
    case Units(from, to)                                    =>
      s"onset ${from.time.fold("undeclared")(_.symbol)} → ${to.time.fold("undeclared")(_.symbol)}"
    case Screen(from, to)          => s"screen ${from.render} → ${to.render}"
    case Image(from, to)           => s"image ${from.render} → ${to.render}"
    case PixelsPerDegree(from, to) =>
      s"${number(from.value)} → ${number(to.value)} px/°"
    case OffScreen(from, to)        => s"off-screen ${offScreen(from)} → ${offScreen(to)}"
    case CorrectionAdded(r)         => s"correction added: ${rule(r)}"
    case CorrectionRemoved(r)       => s"correction removed: ${rule(r)}"
    case CorrectionsReordered(_, _) => "corrections reordered"

  private def inSource(source: MappedSource, phrase: String): String = source match
    case MappedSource.Fixations => phrase
    case MappedSource.Inventory => s"inventory $phrase"

  /** "4 trials change status", "1 trial changes status". */
  def statusCount(n: Int): String =
    if n == 1 then "1 trial changes status" else s"$n trials change status"

  /** "overlap → admitted 3, admitted → no-fixations 1". */
  def transitions(changes: StatusChanges): String =
    changes.transitions
      .map(t => s"${status(t.from)} → ${status(t.to)} ${t.count}")
      .mkString(", ")

  /** The diff as the Data history reads it: one phrase per change, in diff
    * order, then the status changes. A column that moved from an attribute
    * to a role in the same source is said once, as the mapping, and an
    * inventory phrase that repeats a fixation phrase is not repeated.
    */
  def summary(diff: DatasetDiff): String =
    val moved = diff.changes.collect { case Mapped(s, _, _, Some(c)) => (s, c) }
    val shown = diff.changes.filterNot {
      case Attribute(s, c, Some(_), None) => moved.contains((s, c))
      case _                              => false
    }
    val fixationPhrases = shown.collect { case m @ Mapped(MappedSource.Fixations, _, _, _) =>
      change(m)
    }
    val phrases = shown.flatMap {
      case m @ Mapped(MappedSource.Inventory, _, _, _) =>
        val own = change(m.copy(source = MappedSource.Fixations))
        Option.unless(fixationPhrases.contains(own))(change(m))
      case other => Some(change(other))
    }
    val status = diff.status match
      case StatusDiff.Compared(c) if c.trials > 0         => Some(statusCount(c.trials))
      case StatusDiff.Compared(_)                         => None
      case StatusDiff.Unavailable(d, _) if d == diff.from =>
        Some(s"trial status vs ${diff.from.label} unavailable")
      case StatusDiff.Unavailable(d, _) => Some(s"trial status of ${d.label} unavailable")
      case StatusDiff.NotRead           => None
    (phrases ++ status).mkString("; ")

  /** Why a figure on `diff.from`'s data is stale (Figures.dc.html): "dataset
    * r3 changed the admission status of 4 trials", else what it changed.
    */
  def staleCause(diff: DatasetDiff): String =
    val to = s"dataset ${diff.to.label}"
    diff.statusChanges.filter(_ > 0) match
      case Some(n) =>
        s"$to changed the admission status of ${if n == 1 then "1 trial" else s"$n trials"}"
      case None =>
        val parts = diff.changes.map {
          case _: SourceBytes | _: SourceRenamed | _: SourceIdentified => "the sources"
          case _: Mapped | _: Attribute | _: InventoryMapped           => "the mapping"
          case _: Units                                                => "the units"
          case _: Screen | _: Image | _: PixelsPerDegree               => "the geometry"
          case _: OffScreen => "the off-screen policy"
          case _: CorrectionAdded | _: CorrectionRemoved | _: CorrectionsReordered =>
            "the corrections"
        }.distinct
        if parts.isEmpty then s"$to is admitted"
        else if parts.size == 1 then s"$to changed ${parts.head}"
        else s"$to changed ${parts.init.mkString(", ")} and ${parts.last}"
