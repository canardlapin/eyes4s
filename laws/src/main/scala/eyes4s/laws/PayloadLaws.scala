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

package eyes4s.laws

import eyes4s.codec.*
import eyes4s.core.Recording
import eyes4s.kernel.Unit2D
import io.circe.Json
import org.scalacheck.Gen
import org.scalacheck.Prop.forAllNoShrink as forAll
import org.typelevel.discipline.Laws

/** Published conformance of the typed payload codecs (`eyes4s.packed-array@1`
  * and `eyes4s.packed-recording@1`): exact element round trips, digest-exact
  * verification and canonical re-packing. Every double is compared through
  * the equivalence the caller supplies, so a test can require exact IEEE
  * bits rather than numeric equality (which would identify `-0.0` and `+0.0`).
  */
trait PayloadLaws extends Laws:
  /** Packing and unpacking `values` in the given layout through `pack` and
    * `unpack`; [[PayloadLaws.shippedArrays]] supplies [[PackedArrays]], and a
    * test supplies others to show that the laws kill a broken implementation.
    */
  def packedArrays[A](
      values: Gen[(PayloadLayout, IArray[A])],
      same: (A, A) => Boolean,
      pack: (PayloadLayout, IArray[A]) => Either[PayloadError, VerifiedPayload],
      unpack: VerifiedPayload => Either[PayloadError, IArray[A]]
  ): RuleSet =
    // Every generated layout is used, empty ones included: nothing is
    // discarded, so a property can fail but never pass by exhaustion.
    val corruptions = for
      (layout, xs) <- values
      at           <- Gen.choose(0, Int.MaxValue)
      delta        <- Gen.choose(1, 255)
    yield (layout, xs, at, delta)
    new SimpleRuleSet(
      "packedArray",
      "unpacking a packed array returns every element exactly" -> forAll(values) {
        (layout, xs) =>
          pack(layout, xs).flatMap(unpack).exists { ys =>
            ys.length == xs.length && xs.indices.forall(i => same(xs(i), ys(i)))
          }
      },
      "the packed bytes have the layout's exact length and verify against their reference" ->
        forAll(values) { (layout, xs) =>
          pack(layout, xs).exists { payload =>
            payload.ref.layout == layout && payload.bytes.length == layout.byteLength &&
            VerifiedPayload
              .verify(payload.ref, payload.bytes)
              .exists(_.bytes.toVector == payload.bytes.toVector)
          }
        },
      "a changed byte or a changed length is refused by the reference" ->
        forAll(corruptions) { (layout, xs, at, delta) =>
          pack(layout, xs).exists { payload =>
            val bytes  = payload.bytes
            val longer = IArray.from(bytes.toVector :+ delta.toByte)
            val grown  = VerifiedPayload.verify(payload.ref, longer) ==
              Left(PayloadError.Length(layout.byteLength, layout.byteLength + 1))
            // An empty payload has no byte to change or drop.
            val changed = bytes.isEmpty || {
              val i       = at % bytes.length
              val flipped = IArray.tabulate(bytes.length)(j =>
                if j == i then (bytes(j) ^ delta).toByte else bytes(j)
              )
              VerifiedPayload.verify(payload.ref, flipped) ==
                Left(PayloadError.Digest(payload.ref.sha256, ByteDigest.sha256(flipped))) &&
                VerifiedPayload.verify(payload.ref, bytes.drop(1)) ==
                Left(PayloadError.Length(layout.byteLength, layout.byteLength - 1))
            }
            grown && changed
          }
        }
    )

  /** The packed recording through `encode` and `decode`
    * ([[PayloadLaws.shippedRecording]] supplies the shipped codec): decoding a
    * recording from its own payloads reproduces it, re-packing the decoded
    * recording reproduces the document and every payload reference (one
    * encoding per recording), and a missing payload is refused rather than
    * filled in.
    */
  def packedRecording[U <: Unit2D](
      recordings: Gen[Recording[U]],
      same: (Recording[U], Recording[U]) => Boolean,
      encode: Recording[U] => Either[CodecError, PackedRecording],
      decode: (Json, PayloadRef => Option[VerifiedPayload]) => Either[CodecError, Recording[U]]
  ): RuleSet =
    def lookup(packed: PackedRecording)(ref: PayloadRef): Option[VerifiedPayload] =
      packed.payloads.find(_.ref == ref)
    new SimpleRuleSet(
      "packedRecording",
      "decoding from its own payloads reproduces the recording" -> forAll(recordings) { r =>
        encode(r).flatMap(p => decode(p.document, lookup(p))).exists(same(r, _))
      },
      "re-packing the decoded recording is canonical" -> forAll(recordings) { r =>
        encode(r).exists { p =>
          decode(p.document, lookup(p))
            .flatMap(encode)
            .exists(q =>
              q.document == p.document && q.payloads.map(_.ref) == p.payloads.map(_.ref)
            )
        }
      },
      // Payloads are content-addressed: two columns with identical bytes and
      // layout are one payload, so a missing payload removes every copy.
      "a missing payload is refused, never filled in" -> forAll(recordings) { r =>
        encode(r).exists { p =>
          p.payloads.nonEmpty && p.payloads.forall { missing =>
            val remaining = p.payloads.filterNot(_.ref == missing.ref)
            decode(p.document, ref => remaining.find(_.ref == ref)).isLeft
          }
        }
      }
    )

object PayloadLaws extends PayloadLaws:
  /** The shipped packed-array implementation for element type `A`. */
  def shippedArrays[A](
      values: Gen[(PayloadLayout, IArray[A])],
      same: (A, A) => Boolean
  )(using PackedElement[A]): RuleSet =
    packedArrays(values, same, PackedArrays.pack[A], PackedArrays.unpack[A])

  /** The shipped packed-recording codec. */
  def shippedRecording[U <: Unit2D](
      codec: PackedRecordingCodec[U],
      recordings: Gen[Recording[U]],
      same: (Recording[U], Recording[U]) => Boolean
  ): RuleSet =
    packedRecording(recordings, same, codec.encode, codec.decode)

  /** The exact IEEE 754 bits of two doubles agree. */
  def sameBits(a: Double, b: Double): Boolean =
    java.lang.Double.doubleToRawLongBits(a) == java.lang.Double.doubleToRawLongBits(b)
