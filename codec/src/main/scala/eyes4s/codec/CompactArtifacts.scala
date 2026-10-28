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

package eyes4s.codec

/** Explicit host limits, with additional decoded-work bounds derived from the
  * structural-row cap. No application budget is increased by this profile.
  */
private[eyes4s] final class CompactArtifactLimits private[codec] (
    val maxRows: Long,
    val maxBytes: Long,
    private[codec] val document: ColumnarDocument.Limits
)

private[eyes4s] object CompactArtifactLimits:
  def of(maxRows: Long, maxBytes: Long): Either[CodecError, CompactArtifactLimits] =
    if maxRows < 1 || maxBytes < 5 then
      Left(
        CodecError.Unsupported(
          "compact artifact limits",
          s"rows=$maxRows must be positive and bytes=$maxBytes must be at least 5"
        )
      )
    else
      val bytes                       = BigInt(maxBytes).min(BigInt(Int.MaxValue)).toInt
      def count(multiplier: Int): Int =
        (BigInt(maxRows) * multiplier).min(BigInt(Int.MaxValue)).toInt
      val limits = ColumnarDocument.Limits(
        bytes = bytes,
        nodes = math.min(count(8), bytes),
        references = count(64),
        textUnits = bytes,
        expandedNodes = (BigInt(maxRows) * 128).min(BigInt(Long.MaxValue)).toLong,
        depth = 128,
        arrayItems = count(1)
      )
      Right(new CompactArtifactLimits(maxRows, maxBytes, limits))

private[eyes4s] final case class CompactArtifactStatistics(
    structuralRecords: Long,
    uniqueNodes: Int,
    expandedNodes: Long
)

private[eyes4s] object CompactArtifacts:
  private[codec] def defaults: Either[CodecError, CompactArtifactLimits] =
    CompactArtifactLimits.of(1000000L, 268435456L)

  private def statistics(doc: ColumnarDocument.Document): CompactArtifactStatistics =
    val s = doc.statistics
    CompactArtifactStatistics(
      s.arrayRecords.toLong + s.objectShapes + s.shapeFields,
      s.nodes,
      s.expandedNodes
    )

  private def admitted(
      doc: ColumnarDocument.Document,
      limits: CompactArtifactLimits
  ): Either[CodecError, ColumnarDocument.Document] =
    val records = statistics(doc).structuralRecords
    Either.cond(
      records <= limits.maxRows,
      doc,
      CodecError.Unsupported(
        "compact structural records",
        s"$records exceeds ${limits.maxRows}"
      )
    )

  private[codec] def decode(
      bytes: IArray[Byte],
      limits: CompactArtifactLimits
  ): Either[CodecError, ColumnarDocument.Document] =
    ColumnarDocument.decode(bytes, limits.document).flatMap(admitted(_, limits))

  def inspect(
      bytes: IArray[Byte],
      limits: CompactArtifactLimits
  ): Either[CodecError, CompactArtifactStatistics] =
    decode(bytes, limits).map(statistics)

  private[codec] def encode(
      document: CanonicalDoc,
      limits: CompactArtifactLimits
  ): Either[CodecError, IArray[Byte]] = for
    graph <- ColumnarDocument.build(document, limits.document).flatMap(admitted(_, limits))
    bytes <- ColumnarDocument.encode(graph, limits.document)
  yield bytes
