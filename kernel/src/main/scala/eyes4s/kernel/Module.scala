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

package eyes4s.kernel

/** A module: an abelian group with scalar multiplication.
  *
  * ==Why this is defined here==
  *
  * `algebra` has no vector-space abstraction to inherit, and Spire is not
  * published for Scala Native 0.5 and defines a length-zero `zero` for arrays,
  * which is incompatible with a carrier whose zero depends on its dimension.
  * `linop4s` defines its own for exactly these reasons; so does this. The
  * kernel's only external dependency remains cats-core.
  */
trait Module[V, K]:
  def zero: V
  def plus(a: V, b: V): V
  def negate(a: V): V
  def scale(k: K, v: V): V

  final def minus(a: V, b: V): V = plus(a, negate(b))

extension [U <: Unit2D](g: Grid[U])

  /** The module of signed surfaces on this grid.
    *
    * Reached through the grid **value**, never summoned as a given. `zero`
    * depends on which grid you are in -- it is a surface of the right size over
    * the right frame -- so a globally summoned instance could not produce one.
    * Two grids in implicit scope would also be ambiguous. This is the same
    * reasoning `linop4s` gives for making a vector space a runtime value.
    */
  def signedModule: Module[Signed[U], Double] =
    new Module[Signed[U], Double]:

      private def build(vs: IArray[Double], step: Provenance.Step): Signed[U] =
        new Signed(g, vs, Provenance(ContentHash.of(vs), Vector(step)))

      val zero: Signed[U] =
        build(IArray.fill(g.size)(0.0), Provenance.Step.text("zero", "grid", g.id.name))

      def plus(a: Signed[U], b: Signed[U]): Signed[U] =
        new Signed(
          g,
          IArray.tabulate(g.size)(i => a.values(i) + b.values(i)),
          Provenance(
            ContentHash.combine(a.provenance.digest, b.provenance.digest),
            Vector(Provenance.Step("plus"))
          )
        )

      def negate(a: Signed[U]): Signed[U] =
        new Signed(
          g,
          IArray.tabulate(g.size)(i => -a.values(i)),
          a.provenance.andThen(Provenance.Step("negate"))
        )

      def scale(k: Double, v: Signed[U]): Signed[U] =
        new Signed(
          g,
          IArray.tabulate(g.size)(i => v.values(i) * k),
          v.provenance.andThen(Provenance.Step.num("scale", "by", k))
        )
