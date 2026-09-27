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

package eyes4s.studio.core.importing

import eyes4s.io.*
import eyes4s.kernel.*
import eyes4s.plan.{AdmissionPolicy, InventoryError, QuarantineCause, TrialKey as CoreTrialKey}
import eyes4s.studio.core.document.{ColumnName, SourceRole}
import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop.{forAll, propBoolean}

/** The key check agrees with eyes4s-io itself (ticket S5.3), on generated
  * fixation tables: labels whose records name several items or occurrences,
  * occurrences in variant spellings (1, 01, +1), and the occurrence in the
  * key or left out.
  *
  *  - With the occurrence in the key, the keys the check reports as item or
  *    occurrence conflicts are exactly the trials `FixationCsv.admit`
  *    quarantines with `ItemConflict` or `OccurrenceConflict`.
  *  - Without it, item conflicts still agree, and each key the check reports
  *    as repeating without Occurrence (the Studio check that blocks) is one
  *    eyes4s trial key holding the records of every presentation: the merge
  *    the check exists to stop.
  *
  * The inventory's repeated keys are the conflicts eyes4s refuses it for.
  */
class TrialKeyAgreementJvmSuite extends munit.ScalaCheckSuite:

  private def get[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)

  private def name(s: String): ColumnName = get(ColumnName.of(s))

  private val screen  = get(Frame.screen("screen", 1920, 1080))
  private val columns = get(FixationColumns.of("ord", "x", "y", "on", "dur", "ns"))

  /** One record: its label's trial, its occurrence (the value and as
    * written), its item.
    */
  final case class Rec(trial: String, value: Int, occurrence: String, item: String)

  private val spellings: Map[Int, Vector[String]] =
    Map(1 -> Vector("1", "01", "+1"), 2 -> Vector("2", "02", "+2"))

  private val records: Gen[Vector[Rec]] =
    for
      labels <- Gen.choose(1, 6)
      recs   <- Gen.sequence[Vector[Vector[Rec]], Vector[Rec]]((1 to labels).map { l =>
        Gen
          .choose(1, 4)
          .flatMap(n =>
            Gen
              .listOfN(
                n,
                for
                  occ   <- Gen.frequency(3 -> 1, 1 -> 2)
                  spell <- Gen.oneOf(spellings(occ))
                  item  <- Gen.frequency(4 -> "a", 1 -> "b")
                yield Rec(s"t$l", occ, spell, item)
              )
              .map(_.toVector)
          )
      })
    yield recs.flatten

  private def csv(recs: Vector[Rec]): String =
    val rows = recs.groupBy(_.trial).toVector.sortBy(_._1).flatMap { (_, rs) =>
      rs.zipWithIndex.map { (r, i) =>
        s"P01,Enc,${r.trial},${r.occurrence},${r.item},${i + 1},960.0,540.0,${(i + 1) * 300},200,100"
      }
    }
    ("p,f,t,o,item,ord,x,y,on,dur,ns" +: rows).mkString("", "\n", "\n")

  private def check(text: String, inKey: Boolean): KeyReport =
    val key = KeyColumns(
      Some(name("p")),
      Some(name("f")),
      Some(name("t")),
      Option.when(inKey)(name("o")),
      Some(name("item"))
    )
    val p = get(CsvSniffer.sniff("f.csv", text))
    get(
      TrialKeyCheck.check(
        "f.csv",
        SourceRole.Fixations,
        p.keys,
        key,
        TrialUnit.Presentation(Some(name("o")))
      )
    )

  private def admit(text: String, inKey: Boolean): FixationImport[CoreTrialKey, Unit2D.Px] =
    val keys = get(FixationKeyReader.trial("p", "f", "t", "item", Option.when(inKey)("o")))
    get(
      FixationCsv.admit(
        text,
        columns,
        keys,
        screen,
        TimestampUnit.Milliseconds,
        AdmissionPolicy.default[CoreTrialKey]
      )
    )

  private def quarantined(imported: FixationImport[CoreTrialKey, Unit2D.Px], item: Boolean) =
    imported.rejected.collect {
      case RejectedFixationRow(_, _, Some(k), FixationRowError.Trial(_, cause)) if (cause match
            case QuarantineCause.ItemConflict(_)       => item
            case QuarantineCause.OccurrenceConflict(_) => !item
            case _                                     => false
          ) =>
        k.trial
    }.toSet

  private def labels(r: KeyReport, items: Boolean): Set[String] =
    r.repeated.collect {
      case k if k.conflict.isInstanceOf[KeyConflict.Items] == items => k.key.trial
    }.toSet

  property("with Occurrence in the key: conflicts are eyes4s's quarantines, cause for cause") {
    forAll(records) { recs =>
      val text     = csv(recs)
      val report   = check(text, inKey = true)
      val imported = admit(text, inKey = true)
      Prop.all(
        (labels(report, items = true) == quarantined(imported, item = true)) :|
          s"items: ${report.repeated} vs ${imported.rejected}",
        (labels(report, items = false) == quarantined(imported, item = false)) :|
          s"occurrences: ${report.repeated} vs ${imported.rejected}",
        (!report.repeatsWithoutOccurrence) :| "no Studio check with Occurrence in the key"
      )
    }
  }

  property(
    "without Occurrence: item conflicts agree, and each blocked key is one eyes4s trial"
  ) {
    forAll(records) { recs =>
      val text     = csv(recs)
      val report   = check(text, inKey = false)
      val imported = admit(text, inKey = false)
      val merged   = labels(report, items = false)
      // The generator's own truth: one item, more than one occurrence value.
      val expected = recs
        .groupBy(_.trial)
        .collect {
          case (t, rs)
              if rs.map(_.item).distinct.size == 1 && rs.map(_.value).distinct.size > 1 =>
            t
        }
        .toSet
      val keysOf = (imported.admitted.map(r => r.key) ++ imported.rejected.flatMap(_.key))
        .groupBy(_.trial)
        .view
        .mapValues(_.distinct)
        .toMap
      Prop.all(
        (labels(report, items = true) == quarantined(imported, item = true)) :| "items",
        quarantined(imported, item = false).isEmpty :| "eyes4s sees no occurrence",
        (merged == expected) :| s"merged $merged, expected $expected",
        merged.forall(t => keysOf.get(t).exists(_.size == 1)) :|
          s"merged $merged into ${keysOf.filter((t, _) => merged(t))}",
        (report.repeatsWithoutOccurrence == merged.nonEmpty) :| "the Studio check blocks"
      )
    }
  }

  test("the inventory: repeated keys are the conflicts eyes4s refuses the inventory for") {
    val text    = KeyStudies.inventory(blocks = true)
    val columns = get(
      TrialInventoryColumns.of(
        get(TrialColumns.of("participant", "phase", "trial", Some("Block"))),
        Some("item")
      )
    )
    val conflicts = TrialInventory.read(text, columns) match
      case Left(FixationImportError.Inventory(errors)) =>
        errors.toVector.collect { case InventoryError.Conflict(p, f, t, rs, _) =>
          // eyes4s numbers the header as record 1.
          KeyLabel(p, f, t) -> rs.map(_ - 1)
        }.toMap
      case other => fail(s"expected eyes4s to refuse the inventory, got $other")
    val key = KeyColumns(
      Some(name("participant")),
      Some(name("phase")),
      Some(name("trial")),
      Some(name("Block"))
    )
    val p      = get(CsvSniffer.sniff("trials.csv", text))
    val report =
      get(TrialKeyCheck.check("trials.csv", SourceRole.Trials, p.keys, key, TrialUnit.Record))
    assertEquals(conflicts.size, 38)
    assertEquals(
      report.repeated.map(k => k.key -> k.trials.map(_.first)).toMap,
      conflicts
    )
  }
