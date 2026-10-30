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

package eyes4s.examples

import eyes4s.compare.Distribution
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

/** Opt-in location-only qualification. No artificial fixation times or durations. */
object CheckMemoryLocations:
  private def get[A, E](result: Either[E, A]): A =
    result.fold(e => throw new IllegalArgumentException(e.toString), identity)

  private def rows(path: Path, header: String): Vector[Vector[String]] =
    val lines = Files.readString(path, UTF_8).linesIterator.toVector
    require(lines.headOption.contains(header), s"Unexpected header in $path")
    val records = lines.tail.map(_.split("\t", -1).toVector)
    require(records.nonEmpty, s"Empty input: $path")
    require(records.forall(_.size == header.split("\t").length), s"Wrong width in $path")
    records

  def main(args: Array[String]): Unit =
    require(
      args.length == 4,
      "Usage: CheckMemoryLocations locations.tsv pairs.tsv output.tsv bins.tsv"
    )
    val frame  = get(Frame.screen("figrim-image-pixels", 1000, 1000))
    val grid   = get(Grid.over(frame, 10, 10))
    val points = rows(Path.of(args(0)), "trial_id\tx_px\ty_px")
      .groupMap(_.head)(r => Pt[Px](r(1).toDouble, r(2).toDouble))
    val binned = points.map { case (id, ps) =>
      require(
        ps.forall(p => p.x.isFinite && p.y.isFinite && frame.contains(p)),
        s"Invalid point in $id"
      )
      val measure = get(PointMeasure.uniform(frame, IArray.from(ps)))
      val bins    = get(measure.binned(grid))
      require(bins.sum == ps.size.toDouble, s"Lost positions in $id")
      id -> (bins, get(get(Surface.intensity(grid, bins, measure.provenance)).normalised))
    }
    val masses  = binned.view.mapValues(_._2).toMap
    val binRows = binned.toVector.sortBy(_._1).flatMap { case (id, (bins, _)) =>
      bins.iterator.zipWithIndex.collect {
        case (count, index) if count > 0 =>
          Vector(id, (index % 10).toString, (index / 10).toString, count.toString)
            .mkString("\t")
      }
    }
    Files.writeString(
      Path.of(args(3)),
      ("trial_id\tbin_x\tbin_y\tcount" +: binRows).mkString("\n") + "\n",
      UTF_8
    )
    val pairs = rows(Path.of(args(1)), "pair_id\tfocal_id\tencoding_id\tcontrol_id")
    require(pairs.map(_.head).distinct.size == pairs.size, "Duplicate pair identifiers")
    val compare = Distribution.cosine[Px]
    val results = pairs.map { r =>
      require(r(2) != r(3), s"Self control in ${r.head}")
      val focal   = masses(r(1))
      val matched = get(compare.compare(focal, masses(r(2)))).value
      val control = get(compare.compare(focal, masses(r(3)))).value
      require(matched.isFinite && control.isFinite, s"Nonfinite score in ${r.head}")
      Vector(r.head, matched.toString, control.toString, (matched - control).toString)
        .mkString("\t")
    }
    Files.writeString(
      Path.of(args(2)),
      ("pair_id\tmatched\tcontrol\tdifference" +: results).mkString("\n") + "\n",
      UTF_8
    )
    println(
      s"Qualified ${points.size} location sets and ${results.size} encoding/retrieval pairs"
    )
