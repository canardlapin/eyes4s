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

package eyes4s.studio.app.figures

import scala.annotation.tailrec

/** One line of a sentence diff. */
enum DiffLine derives CanEqual:
  case Same(text: String)
  case Removed(text: String)
  case Added(text: String)

/** A sentence-by-sentence diff of two methods texts. */
object TextDiff:
  // A sentence ends at '.', '!' or '?' followed by space and a capital, a
  // digit or an opening bracket or quote; "0.46°" and "D = M − B." inside a
  // sentence do not end one. Scanned by hand: Scala.js has no look-behind.
  private def opens(c: Char): Boolean =
    c.isUpper || c.isDigit || c == '(' || c == '“' || c == '"'

  def sentences(text: String): Vector[String] =
    val t                                                                    = text.trim
    @tailrec def go(start: Int, i: Int, out: Vector[String]): Vector[String] =
      if i >= t.length then out :+ t.substring(start)
      else if ".!?".contains(t(i)) && i + 1 < t.length && t(i + 1).isWhitespace then
        val next = (i + 1 until t.length).find(k => !t(k).isWhitespace).getOrElse(t.length)
        if next < t.length && opens(t(next)) then
          go(next, next, out :+ t.substring(start, i + 1))
        else go(start, i + 1, out)
      else go(start, i + 1, out)
    go(0, 0, Vector.empty).map(_.trim).filter(_.nonEmpty)

  /** `from` to `to`, by the longest common subsequence of their sentences. */
  def apply(from: String, to: String): Vector[DiffLine] =
    val a = sentences(from)
    val b = sentences(to)
    // lcs(i)(j): the common length of a.drop(i) and b.drop(j).
    val lcs = Array.ofDim[Int](a.size + 1, b.size + 1)
    for
      i <- a.indices.reverse
      j <- b.indices.reverse
    do
      lcs(i)(j) =
        if a(i) == b(j) then lcs(i + 1)(j + 1) + 1 else lcs(i + 1)(j).max(lcs(i)(j + 1))
    @tailrec def walk(i: Int, j: Int, out: Vector[DiffLine]): Vector[DiffLine] =
      if i < a.size && j < b.size && a(i) == b(j) then
        walk(i + 1, j + 1, out :+ DiffLine.Same(a(i)))
      else if i < a.size && (j >= b.size || lcs(i + 1)(j) >= lcs(i)(j + 1)) then
        walk(i + 1, j, out :+ DiffLine.Removed(a(i)))
      else if j < b.size then walk(i, j + 1, out :+ DiffLine.Added(b(j)))
      else out
    walk(0, 0, Vector.empty)

  /** How many sentences a diff changes: each run of changes counts its
    * larger side, so one rewritten sentence is one.
    */
  def changed(diff: Vector[DiffLine]): Int =
    def same(l: DiffLine) = l match
      case DiffLine.Same(_) => true
      case _                => false
    @tailrec def go(rest: Vector[DiffLine], total: Int): Int =
      val (run, tail) = rest.dropWhile(same).span(l => !same(l))
      if run.isEmpty then total
      else
        val removed = run.count {
          case DiffLine.Removed(_) => true
          case _                   => false
        }
        go(tail, total + removed.max(run.size - removed))
    go(diff, 0)

/** The methods text of one figure as the author left it (ticket S9.4).
  *
  * `base` is the generated text the author's edits started from; `edited` is
  * their text. Regenerating against an edited draft never replaces it: the
  * new generated text waits in `pending`, with a diff, until the author keeps
  * their edits (the new text becomes the base) or takes the generated text.
  */
final case class MethodsDraft(base: String, edited: String, pending: Option[String])
    derives CanEqual:
  def isEdited: Boolean = edited != base
