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

package eyes4s.studio.app.compare

import eyes4s.studio.app.text.{Format, PairedText, PairedTextId}
import eyes4s.studio.core.backend.{ReportRole, ReportView, Response}

/** One paired participant's difference: its mean D in the minuend group
  * minus its mean D in the subtrahend group, both as the report served
  * them (unrounded).
  */
final case class PairedDifference(participant: String, d: Double) derives CanEqual

/** The Results headline's paired descriptive (design review ticket 27;
  * DESIGN_SPEC §15): for every participant with a mean D in both levels of
  * the report's contrast, its `minuend` mean minus its `subtrahend` mean,
  * in the report's participant order. `unpaired` lists the participants
  * with a mean in one level only, who are left out of the pairing.
  *
  * Every summary is computed from the unrounded participant means and only
  * then rounded for display; rounding the participant means first would
  * double-round (the fixture's +0.16 would read +0.15). It is descriptive
  * only: no interval, no test.
  */
final case class PairedDescriptive(
    minuend: Response,
    subtrahend: Response,
    differences: Vector[PairedDifference],
    unpaired: Vector[String]
) derives CanEqual:

  /** The paired participants: the n of every summary below. */
  def pairedN: Int = differences.size

  /** The mean over paired participants of their differences; none when no
    * participant is paired.
    */
  def mean: Option[Double] =
    Option.when(differences.nonEmpty)(differences.map(_.d).sum / differences.size)

  /** The smallest difference and its participant (the first, on a tie). */
  def lowest: Option[PairedDifference] = differences.minByOption(_.d)

  /** The largest difference and its participant (the first, on a tie). */
  def highest: Option[PairedDifference] = differences.maxByOption(_.d)

  /** How many paired participants have a strictly higher mean in the
    * minuend: a difference of exactly zero is not higher.
    */
  def higher: Int = differences.count(_.d > 0.0)

object PairedDescriptive:

  /** The paired descriptive of `report`'s D contrast, from the participant
    * means the report serves; none when the report has no D contrast (an
    * ungrouped spec).
    */
  def of(report: ReportView): Option[PairedDescriptive] =
    report.contrast(ReportRole.Difference).map { contrast =>
      val means = report.participants.filter(_.role == ReportRole.Difference)
      def in(level: Response): Vector[(String, Double)] =
        means.collect {
          case p if p.group.contains(level) && p.value.isDefined => (p.participant, p.value.get)
        }
      val (minuend, subtrahend) = (in(contrast.minuend), in(contrast.subtrahend))
      val other                 = subtrahend.toMap
      val differences           = minuend.collect {
        case (p, m) if other.contains(p) => PairedDifference(p, m - other(p))
      }
      val paired   = differences.map(_.participant).toSet
      val unpaired = (minuend.map(_._1) ++ subtrahend.map(_._1)).distinct.filterNot(paired)
      PairedDescriptive(contrast.minuend, contrast.subtrahend, differences, unpaired)
    }

/** What the Results headline's paired block shows: its key, the rounded
  * mean difference and how many participants are higher (none when nothing
  * is paired), the note (range, rounding, descriptive only, the unpaired
  * count) and the block read as one sentence.
  */
final case class PairedDescriptiveVM(
    heading: String,
    value: Option[String],
    higher: Option[String],
    note: String,
    spoken: String
) derives CanEqual

object PairedDescriptiveVM:

  /** The decimals the board prints a mean D and its range at. */
  val Decimals: Int = 2

  /** The block at `scale`, the σ as the summary names it ("σ 2°"). */
  def of(paired: PairedDescriptive, scale: String): PairedDescriptiveVM =
    import PairedTextId.*
    def signed(v: Double)     = Format.signed(v, Decimals)
    val (minuend, subtrahend) = (paired.minuend.label, paired.subtrahend.label)
    val n                     = paired.pairedN.toString
    val heading               = PairedText(Heading, minuend, subtrahend, scale)
    val unpaired              =
      Option.when(paired.unpaired.nonEmpty)(PairedText.unpaired(paired.unpaired.size))
    def spoken(sentence: String) =
      unpaired.fold(sentence)(PairedText(SpokenUnpaired, sentence, _))
    val summary = for
      mean <- paired.mean
      lo   <- paired.lowest
      hi   <- paired.highest
    yield (mean, lo, hi)
    summary match
      case None =>
        PairedDescriptiveVM(
          heading,
          None,
          None,
          (Vector(PairedText(NonePaired, minuend, subtrahend)) ++ unpaired)
            .mkString(PairedText(Separator)),
          spoken(PairedText(SpokenNonePaired, minuend, subtrahend))
        )
      case Some((mean, lo, hi)) =>
        val higher = PairedText(Higher, minuend, paired.higher.toString, n)
        val range  =
          PairedText(Range, signed(lo.d), lo.participant, signed(hi.d), hi.participant)
        PairedDescriptiveVM(
          heading,
          Some(signed(mean)),
          Some(higher),
          (Vector(range, PairedText(DescriptiveOnly)) ++ unpaired)
            .mkString(PairedText(Separator)),
          spoken(
            PairedText(
              Spoken,
              minuend,
              subtrahend,
              n,
              signed(mean),
              paired.higher.toString,
              n,
              signed(lo.d),
              lo.participant,
              signed(hi.d),
              hi.participant
            )
          )
        )
