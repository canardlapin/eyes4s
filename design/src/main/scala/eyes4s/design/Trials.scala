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

package eyes4s.design

/** One typed observation in a study.
  *
  * Every field used for identity, matching, stratification, occasion, or
  * participant scope belongs in `K`. `M` is carried unchanged and never
  * interpreted by eyes4s. `A` is the value an analysis transforms or compares.
  */
final case class Trial[K, M, A](key: K, meta: M, value: A) derives CanEqual

/** A failed value transformation, retaining the complete source operand.
  *
  * The row index distinguishes repeated keys. The original trial is returned
  * rather than silently disappearing when [[Trials.traverseV]] keeps the
  * successful rows.
  */
final case class TrialTransformFailure[K, M, A, E](
    index: Int,
    trial: Trial[K, M, A],
    error: E
) derives CanEqual:
  def key: K   = trial.key
  def meta: M  = trial.meta
  def value: A = trial.value

/** An ordered collection whose matching dimensions are statically typed.
  *
  * `Trials` deliberately permits empty collections and repeated keys.
  * Repetition is meaningful study data; an operation requiring unique matches
  * reports ambiguity explicitly rather than weakening this container.
  *
  * No operation accepts a string naming a column. Pairing code receives `K`
  * and typed [[Projection]] values, so a grouping dimension cannot be omitted
  * merely because a caller misspelled or forgot a data-frame column.
  */
final case class Trials[K, M, A](rows: Vector[Trial[K, M, A]]) derives CanEqual:

  def size: Int         = rows.size
  def isEmpty: Boolean  = rows.isEmpty
  def nonEmpty: Boolean = rows.nonEmpty

  def get(index: Int): Option[Trial[K, M, A]] =
    rows.lift(index)

  def filter(predicate: Trial[K, M, A] => Boolean): Trials[K, M, A] =
    Trials(rows.filter(predicate))

  def filterKey(predicate: K => Boolean): Trials[K, M, A] =
    filter(row => predicate(row.key))

  def mapV[B](f: A => B): Trials[K, M, B] =
    Trials(rows.map(row => Trial(row.key, row.meta, f(row.value))))

  /** Transform values independently, retaining successful rows and every
    * failed source operand.
    *
    * Both result vectors preserve input order. A failure changes the successful
    * result's cardinality, but the omitted row remains available in the
    * corresponding [[TrialTransformFailure]].
    */
  def traverseV[E, B](
      f: A => Either[E, B]
  ): (Trials[K, M, B], Vector[TrialTransformFailure[K, M, A, E]]) =
    val successful = Vector.newBuilder[Trial[K, M, B]]
    val failures   = Vector.newBuilder[TrialTransformFailure[K, M, A, E]]

    rows.zipWithIndex.foreach { case (row, index) =>
      f(row.value) match
        case Right(value) => successful += Trial(row.key, row.meta, value)
        case Left(error)  => failures += TrialTransformFailure(index, row, error)
    }

    (Trials(successful.result()), failures.result())

end Trials

object Trials:
  def empty[K, M, A]: Trials[K, M, A] =
    Trials(Vector.empty)

  def one[K, M, A](key: K, meta: M, value: A): Trials[K, M, A] =
    Trials(Vector(Trial(key, meta, value)))
