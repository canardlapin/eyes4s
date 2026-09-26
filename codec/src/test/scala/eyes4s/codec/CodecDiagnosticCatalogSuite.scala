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

import eyes4s.kernel.ContentHash
import eyes4s.plan.*
import io.circe.Json

/** The codec's code table beside the plan's: total over the compiler's case
  * lists, one sample per case, every field kept, codes unique across both
  * tables and pinned on the JVM and Scala.js, and no error type reachable
  * from any sample (CodecError included) left without a cataloged family.
  */
class CodecDiagnosticCatalogSuite extends munit.FunSuite:
  import CodecDiagnostics.given

  private val codec = CodecDiagnosticSamples.all
  private val all   =
    DiagnosticSamples.all ++ codec ++ eyes4s.results.ResultsDiagnosticSamples.all

  private val PinnedCount  = 117
  private val PinnedDigest = "4474cd96d19a2abe"

  private val alignment = DiagnosticAlignment(
    all,
    eyes4s.results.ResultsDiagnosticSamples.structured.orElse {
      case v: ByteDigest       => CodecDiagnosticSupport.digest(v)
      case v: ArtifactName     => CodecDiagnosticSupport.entry(v)
      case v: ArtifactRole     => CodecDiagnosticSupport.role(v)
      case v: MediaKind        => CodecDiagnosticSupport.media(v)
      case v: ElementKind      => CodecDiagnosticSupport.element(v)
      case v: PayloadLayout    => CodecDiagnosticSupport.layout(v)
      case v: PayloadRef       => CodecDiagnosticSupport.payloadRef(v)
      case v: ManifestRelation => CodecDiagnosticSupport.relation(v)
      case v: Json             => CodecDiagnosticSupport.json(v)
    },
    {
      case (v: Char, Operand.Text(x)) => x == v.toString
      // Keys inside a codec error are erased to their runtime values.
      case (v: StudyKey, Operand.Key(x: ErasedKey)) => x.value == v
    }
  )

  test("every codec family is sampled, in catalog order, through its own Diagnose instance") {
    assertEquals(codec.map(_.family.name), CodecDiagnosticCatalog.families.map(_.name))
    codec.zip(CodecDiagnosticCatalog.families).foreach { (sampled, family) =>
      assert(sampled.family eq family, sampled.enumName)
    }
    assertEquals(
      all.map(_.family.name),
      (CodecDiagnosticCatalog.all ++ eyes4s.results.ResultsDiagnosticCatalog.families)
        .map(_.name)
    )
  }

  test("each family's labels are the compiler's cases, and every case is sampled once") {
    codec.foreach { family =>
      assertEquals(family.family.labels, family.labels, family.enumName)
      assertEquals(
        family.samples.map(_._1.ordinal).sorted,
        family.labels.indices.toVector,
        family.enumName
      )
      family.samples.foreach { (sample, diagnostic) =>
        assertEquals(diagnostic.code, family.family.codes(sample.ordinal), s"$sample")
      }
    }
  }

  test("codes are unique across the plan and codec tables") {
    val codes = CodecDiagnosticCatalog.all.flatMap(_.codes).map(_.render)
    assertEquals(codes.distinct.size, codes.size)
    assertEquals(
      CodecDiagnosticCatalog.all.map(_.name).distinct.size,
      CodecDiagnosticCatalog.all.size
    )
  }

  test("every projection keeps every field, and every reachable error type is cataloged") {
    assertEquals(alignment.problems, Vector.empty)
  }

  test("codec samples give same-typed fields distinct values, so a swap is detected") {
    val repeating = codec.flatMap(_.samples).collect {
      case (sample, _) if {
            val values = sample.productIterator.toVector.collect {
              case v: Int    => "Number" -> v.toDouble.toString
              case v: Double => "Number" -> v.toString
              case v: Long   => "Long"   -> v.toString
              case v: String => "String" -> v
            }
            values.distinct.size != values.size
          } =>
        sample.toString
    }
    assertEquals(repeating, Vector.empty)
  }

  test("the codec code table is pinned and so identical on the JVM and Scala.js") {
    val rendered = CodecDiagnosticCatalog.codes.map(_.render)
    assertEquals(rendered.size, PinnedCount)
    assertEquals(ContentHash.ofString(rendered.mkString("\n")).render, PinnedDigest)
    assertEquals(rendered.head, "codec.invalid-json")
  }

  test("decode and resolution failures name the entry, path, trial or record they concern") {
    val key   = DiagnosticSamples.k1
    val entry = Diagnostic.of(
      ResolveError.Decode(
        CodecDiagnosticSamples.name("result"),
        CodecError.Entry(
          "scales[0].analyses.matched.entries[0]",
          CodecError.Reconstruction(
            eyes4s.design.ReconstructionError.Denominator(key, 1, 0, 5)
          )
        )
      )
    )
    assertEquals(entry.code.render, "resolve.decode")
    // The codec cannot state the key type; the application narrows to its own.
    assertEquals(entry.keys, Vector(new ErasedKey(key)))
    assertEquals(entry.narrow[Int], None)
    val typed = entry.narrow[StudyKey].getOrElse(fail("the keys are study keys"))
    assertEquals(typed.affectedTrials, Vector(key))
    assertEquals(
      typed.subject,
      Vector(
        Locus.Entry("result"),
        Locus.Path("scales[0].analyses.matched.entries[0]"),
        Locus.Trial(key)
      )
    )
    assertEquals(typed.keys, Vector(key))
    val relation = Diagnostic.of(
      ResolveError.Relation(
        ManifestRelation.LedgerOf(
          CodecDiagnosticSamples.name("ledger"),
          CodecDiagnosticSamples.name("input")
        ),
        RelationMismatch.Admission(AdmissionError.QuarantineAdmitted(4, 7))
      )
    )
    assertEquals(
      relation.subject,
      Vector(Locus.Relation("ledger-of", "ledger"), Locus.Record(4))
    )
  }
