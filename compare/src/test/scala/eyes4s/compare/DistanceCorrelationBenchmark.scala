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

package eyes4s.compareconsumer

import eyes4s.compare.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px

/** Opt-in qualification, never a wall-clock assertion in an ordinary test. */
object DistanceCorrelationBenchmark:
  def main(args: Array[String]): Unit =
    val sizes = if !args.contains("--smoke") then Vector(16384, 65536, 262144, 1048576)
    else Vector(16384, 65536, 262144)
    def get[E, A](value: Either[E, A]): A = value.fold(e => sys.error(e.toString), identity)
    sizes.foreach { n =>
      val g                = get(Grid.over(get(Frame.screen(s"dcor-benchmark-$n", n, 1)), n, 1))
      val rng              = new scala.util.Random(20261014L + n)
      def mass(): Mass[Px] = get(
        Surface
          .intensity(g, IArray.fill(n)(rng.nextDouble()), Provenance.raw(ContentHash.empty))
          .flatMap(_.normalised)
      )
      val a       = mass()
      val b       = mass()
      val measure = Distribution.distanceCorrelation[Px]
      get(measure.compare(a, b))
      (0 until 3).foreach { iteration =>
        val start   = System.nanoTime()
        val result  = get(measure.compare(a, b)).value
        val elapsed = (System.nanoTime() - start) / 1e6
        val bits    = java.lang.Double.doubleToLongBits(result).toString
        println(
          s"DCOR_BENCHMARK={\"cells\":$n,\"iteration\":$iteration,\"milliseconds\":$elapsed,\"score\":$result,\"bits\":\"$bits\"}"
        )
      }
    }
