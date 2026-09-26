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

package eyes4s.io

import eyes4s.codec.*
import eyes4s.kernel.ContentHash
import eyes4s.laws.*
import eyes4s.plan.*
import io.circe.Json

/** The io and laws code tables beside the plan's and codec's: total over the
  * compiler's case lists, one generated sample per case, every field kept,
  * codes unique across every table, pinned on the JVM and Scala.js, and every
  * error reachable from any sample cataloged.
  */
class IoDiagnosticCatalogSuite extends munit.FunSuite:
  import IoDiagnostics.given

  private val all = IoDiagnosticSamples.all

  private val IoCount    = 183
  private val IoDigest   = "1cadee57d23ba6be"
  private val LawsCount  = 21
  private val LawsDigest = "cec5dc887ffa1c46"

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
      case v: Sha256           => Operand.Artifact(v.hex)
      case v: SourceIdentity   => Operand.Artifact(v.digest)
      case v: IdentityChanges  =>
        Operand.Items(v.values.toVector.sortBy(_.ordinal).map(x => Operand.Token(x.toString)))
    },
    { case (v: StudyKey, Operand.Key(x: ErasedKey)) => x.value == v }
  )

  test("every io and laws family is sampled, in catalog order, through its own instance") {
    assertEquals(
      IoDiagnosticSamples.io.map(_.family.name),
      IoDiagnosticCatalog.families.map(_.name)
    )
    IoDiagnosticSamples.io.zip(IoDiagnosticCatalog.families).foreach { (sampled, family) =>
      assert(sampled.family eq family, sampled.enumName)
    }
    assertEquals(
      IoDiagnosticSamples.laws.map(_.family.name),
      LawsDiagnosticCatalog.families.map(_.name)
    )
    IoDiagnosticSamples.laws.zip(LawsDiagnosticCatalog.families).foreach { (sampled, family) =>
      assert(sampled.family eq family, sampled.enumName)
    }
  }

  test("each family's labels are the compiler's cases, and every case projects its own code") {
    (IoDiagnosticSamples.io ++ IoDiagnosticSamples.laws).foreach { family =>
      assertEquals(family.family.labels, family.labels, family.enumName)
      assertEquals(
        family.samples.map(_._1.ordinal),
        family.labels.indices.toVector,
        family.enumName
      )
      family.samples.foreach { (sample, diagnostic) =>
        assertEquals(diagnostic.code, family.family.codes(sample.ordinal), s"$sample")
        assertEquals(diagnostic.source, DiagnosticSource.EyesCore)
      }
    }
  }

  test("codes and family names are unique across every catalog") {
    val families = all.map(_.family)
    val codes    = families.flatMap(_.codes).map(_.render)
    assertEquals(codes.distinct.size, codes.size)
    assertEquals(families.map(_.name).distinct.size, families.size)
    assert(codes.forall(!_.contains("uncatalogued")))
  }

  test("every projection keeps every field, and every reachable error type is cataloged") {
    assertEquals(alignment.problems, Vector.empty)
  }

  test("generated samples give same-typed fields distinct values, so a swap is detected") {
    val repeating =
      (IoDiagnosticSamples.io ++ IoDiagnosticSamples.laws).flatMap(_.samples).collect {
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

  test("the io and laws code tables are pinned and so identical on the JVM and Scala.js") {
    val io = IoDiagnosticCatalog.codes.map(_.render)
    assertEquals(io.size, IoCount)
    assertEquals(ContentHash.ofString(io.mkString("\n")).render, IoDigest)
    assertEquals(io.head, "fixation-import.csv")
    val laws = LawsDiagnosticCatalog.codes.map(_.render)
    assertEquals(laws.size, LawsCount)
    assertEquals(ContentHash.ofString(laws.mkString("\n")).render, LawsDigest)
  }

  test("an import error names the record, line or rows it concerns") {
    val row = Diagnostic.of(FixationImportError.Csv(TidyCsvError.WrongColumnCount(7, 9, 8)))
    assertEquals(row.code.render, "fixation-import.csv")
    assertEquals(row.subject, Vector(Locus.Record(7)))
    assertEquals(row.causes.map(_.code.render), Vector("tidy-csv.wrong-column-count"))
    assertEquals(
      Diagnostic.of(FixationImportError.Incomplete(Vector(4, 11))).subject,
      Vector(Locus.Records(Vector(4, 11)))
    )
    val line = Diagnostic.of(AscSourceLineError.LineTooLong("session.asc", 42L, 8, 9))
    assertEquals(line.subject, Vector(Locus.Line("session.asc", 42L)))
    assertEquals(
      line.operands.map(_._1),
      Vector("source", "line", "limit", "actual")
    )
    val framed = Diagnostic.of(
      AscStreamConfigurationError.InvalidLineLimit(
        0,
        AscSourceLineError.NonPositiveLineLimit(0)
      )
    )
    assertEquals(
      framed.causes.map(_.code.render),
      Vector("asc-source-line.non-positive-line-limit")
    )
    val corpus = Diagnostic.of(EyeLinkCorpusError.WrongFieldCount("corpus.tsv", 3, 5, 4))
    assertEquals(corpus.subject, Vector(Locus.Line("corpus.tsv", 3L)))
  }

  test("every sampled case that names a source and a line has that line as its subject") {
    val lined = IoDiagnosticSamples.io.flatMap(_.samples).collect {
      case (sample, diagnostic)
          if sample.productElementNames.contains("source") &&
            sample.productElementNames.contains("line") =>
        val fields = sample.productElementNames.zip(sample.productIterator).toMap
        val line   = fields("line") match
          case n: Int  => n.toLong
          case n: Long => n
          case other   => fail(s"line $other")
        assert(
          diagnostic.subject.contains(Locus.Line(fields("source").toString, line)),
          s"$sample has subject ${diagnostic.subject}"
        )
        sample
    }
    assert(lined.size >= 12, lined.size)
    val records = IoDiagnosticSamples.io
      .filter(_.family eq IoDiagnosticCatalog.tidyCsv)
      .flatMap(_.samples)
      .collect {
        case (sample, diagnostic) if sample.productElementNames.contains("line") =>
          assertEquals(
            diagnostic.subject,
            Vector(Locus.Record(sample.productElement(0).asInstanceOf[Int]))
          )
          sample
      }
    assertEquals(records.size, 5)
  }

  test("a family that wraps its own errors projects the inner one through itself") {
    val inner = TidyResultError.BlankWarning("s1", 2, " ")
    val outer = Diagnostic.of(TidyResultError.InvalidOperationParameters("s1", 0, "op", inner))
    assertEquals(outer.code.render, "tidy-result.invalid-operation-parameters")
    assertEquals(outer.causes.map(_.code.render), Vector("tidy-result.blank-warning"))
    assertEquals(outer.causes, Vector(Diagnostic.of(inner)))
  }

  test("an export error that wraps a decode failure keeps its trial keys, erased") {
    import eyes4s.design.ReconstructionError
    val key = DiagnosticSamples.k1
    val d   = Diagnostic.of(
      ResultExportError.Score(
        ContrastExportError.Codec(
          CodecError.Reconstruction(ReconstructionError.Denominator(key, 1, 2, 3))
        )
      )
    )
    assertEquals(d.affectedTrials, Vector(new ErasedKey(key)))
    val typed = d.narrow[StudyKey].getOrElse(fail("keys are study keys"))
    assertEquals(typed.subject, Vector(Locus.Trial(key)))
    assertEquals(typed.affectedTrials, Vector(key))
  }
