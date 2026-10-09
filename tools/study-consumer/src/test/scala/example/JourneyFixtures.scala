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

package example

/** The journey's table and its independent numerical oracles. */
object JourneyFixtures:

  /** Microseconds beyond 2^53: not representable as a double, so any route
    * that passes a time through floating point changes it.
    */
  val late: Long = (1L << 53) + 1

  private val rows = ConsumerFixtures.csv.linesIterator.toVector

  private def fields(line: String) = line.split(',').toVector

  /** The pinned matched/control table, with every onset of `s2/c/recall`
    * moved by `late`. Onsets do not enter duration-weighted maps, so the
    * numerical oracles below are unchanged.
    */
  val table: String = rows
    .map { line =>
      val f = fields(line)
      if f.take(3) == Vector("s2", "c", "recall") then
        f.updated(6, (f(6).toLong + late).toString).mkString(",")
      else line
    }
    .mkString("", "\n", "\n")

  /** One admitted reference has only one fixation. Dropping the first
    * fixation empties s1/b/encode while the other eleven trials remain usable.
    * All 45 source records are valid; this is an estimation failure, not a
    * rejected input record or a whole-scale failure.
    */
  val mixed: String = table.linesIterator
    .filter { line =>
      val f = fields(line)
      f.take(3) != Vector("s1", "b", "encode") || f(3) == "0"
    }
    .mkString("", "\n", "\n")

  /** The same table with a negative duration in fixation 2 of `s1/b/encode`. */
  val broken: String = table.linesIterator
    .map { line =>
      val f = fields(line)
      if f.take(4) == Vector("s1", "b", "encode", "2") then
        f.updated(7, "-200000").mkString(",")
      else line
    }
    .mkString("", "\n", "\n")

  /** Logical CSV records (the header is record 1) of one trial's rows,
    * read from the table text rather than from any eyes4s value.
    */
  def records(participant: String, image: String, phase: String): Vector[Int] =
    table.linesIterator.zipWithIndex.collect {
      case (line, index) if fields(line).take(3) == Vector(participant, image, phase) =>
        index + 1
    }.toVector

  /** Onsets of one trial as written in the table. */
  def onsets(participant: String, image: String, phase: String): Vector[Long] =
    table.linesIterator
      .map(fields)
      .collect { case f if f.take(3) == Vector(participant, image, phase) => f(6).toLong }
      .toVector

  final case class Rational(numerator: Long, denominator: Long):
    def value: Double = numerator.toDouble / denominator.toDouble

  /** Binned cosine reductions: matched mean, control mean and their
    * difference as exact rationals, transcribed from
    * `tools/r-parity/fixtures/exact.json`. verify.py checks the journey's printed
    * values against that file independently.
    */
  val binned: Vector[(String, (Rational, Rational, Rational))] = Vector(
    "s1/a/recall" -> (Rational(1, 1), Rational(18, 25), Rational(7, 25)),
    "s1/b/recall" -> (Rational(4, 5), Rational(9, 10), Rational(-1, 10)),
    "s1/c/recall" -> (Rational(4, 5), Rational(41, 50), Rational(-1, 50)),
    "s2/a/recall" -> (Rational(21, 25), Rational(41, 50), Rational(1, 50)),
    "s2/b/recall" -> (Rational(1, 1), Rational(17, 25), Rational(8, 25)),
    "s2/c/recall" -> (Rational(21, 25), Rational(43, 50), Rational(-1, 50))
  )

  /** Gaussian contrast targets by sigma, from the 60-digit closed-form
    * oracle, relabelled from the generated fixture's item numbers.
    */
  val gaussian: Vector[(Double, Map[String, Double])] =
    ConsumerFixtures.gaussianTargets.map { case (sigma, targets) =>
      sigma -> targets.map { case (key, value) =>
        val parts = key.split('/')
        s"${parts(0)}/${"abc" (parts(1).toInt - 1)}/${parts(2)}" -> value
      }
    }
