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

package eyes4s.studio.core.artifacts

import cats.syntax.all.*
import eyes4s.codec.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.studio.core.document.*
import eyes4s.studio.core.real.RealPlan
import eyes4s.studio.core.runs.RunArchive
import java.nio.charset.StandardCharsets.UTF_8

/** A resolver-verified complete native closure. Job handles are deliberately
  * absent; bead q-native-stored-completion records the storage/binding seam.
  */
final class NativeArtifactPackage private (
    val facts: NativeBindingFacts,
    val manifest: ScientificManifest,
    val manifestAddress: ByteDigest,
    val budget: NativeArtifactBudget,
    val files: Vector[(ArtifactName, IndexedSeq[Byte])],
    val archive: RunArchive,
    private[core] val checked: CheckedNativeSnapshot
):
  def bytes(name: ArtifactName): Option[IndexedSeq[Byte]] = files.collectFirst {
    case (`name`, data) => data
  }

object NativeArtifactPackage:
  val FactsName: ArtifactName    = ArtifactName.of("studio.binding-facts").toOption.get
  val ManifestName: ArtifactName = ArtifactName.of("manifest").toOption.get
  val PlanName: ArtifactName     = ArtifactName.of("plan").toOption.get
  val InputName: ArtifactName    = ArtifactName.of("input").toOption.get
  val LedgerName: ArtifactName   = ArtifactName.of("ledger").toOption.get
  val ResultName: ArtifactName   = ArtifactName.of("result").toOption.get

  private def layout[A](operation: String)(value: Either[CodecError, A]) =
    value.leftMap(e => NativeArtifactError.Layout(operation, e.message))
  private def decoded(
      operation: String,
      data: IndexedSeq[Byte]
  ): Either[NativeArtifactError, io.circe.Json] =
    val text      = String(data.toArray, UTF_8)
    val roundTrip = text.getBytes(UTF_8)
    if roundTrip.length != data.length || roundTrip.indices.exists(i => roundTrip(i) != data(i))
    then Left(NativeArtifactError.Layout(operation, "Invalid UTF-8 bytes."))
    else
      io.circe.parser.parse(text).leftMap(e => NativeArtifactError.Layout(operation, e.message))
  private def limit(
      resource: String,
      found: BigInt,
      maximum: BigInt
  ): Either[NativeArtifactError, Unit] =
    Either.cond(
      found <= maximum,
      (),
      NativeArtifactError.UnscopedBudget(resource, found, maximum)
    )
  private def fieldRows(json: io.circe.Json): BigInt = json.arrayOrObject(
    BigInt(0),
    _.foldLeft(BigInt(0))((total, value) => total + fieldRows(value)),
    fields =>
      fields.toVector.foldLeft(BigInt(0)) { case (total, (name, value)) =>
        val here = if Set("rows", "records", "estimation")(name) then
          value.asArray.fold(BigInt(0))(a => BigInt(a.size))
        else BigInt(0)
        total + here + fieldRows(value)
      }
  )

  /** Verifies exact bytes, all library relations, and the declared canonical
    * identities after storage. Unlisted files and incomplete closures refuse.
    */
  def verify(
      files: Vector[(ArtifactName, IndexedSeq[Byte])],
      budget: NativeArtifactBudget = NativeArtifactBudget.Default
  ): Either[NativeArtifactError, NativeArtifactPackage] =
    val names = files.map(_._1)
    // The budget and name checks run before copying any supplied body.
    lazy val owned  = files.map((name, bytes) => name -> NativeArtifactBytes.copyOf(bytes))
    lazy val byName = owned.toMap
    def need(name: ArtifactName) =
      byName.get(name).toRight(NativeArtifactError.Layout("entry", s"Missing $name."))
    for
      _ <- limit("entries", BigInt(files.size), BigInt(budget.maxEntries))
      _ <- limit(
        "total bytes",
        files.foldLeft(BigInt(0))((n, file) => n + file._2.size),
        BigInt(budget.maxTotalBytes)
      )
      _ <- Either.cond(
        names.distinct.size == names.size,
        (),
        NativeArtifactError.Layout("entry names", "Duplicate names.")
      )
      factBytes     <- need(FactsName)
      factJson      <- decoded("facts", factBytes)
      factsCodec    <- layout("facts codec")(NativeBindingFacts.codec)
      facts         <- layout("facts decode")(factsCodec.decode(factJson))
      manifestBytes <- need(ManifestName)
      manifestJson  <- decoded("manifest", manifestBytes)
      graph         <- layout("manifest decode")(ScientificManifest.codec.decode(manifestJson))
      _             <- Either.cond(
        graph.entries.map(_.name).toSet == names.toSet -- Set(FactsName, ManifestName),
        (),
        NativeArtifactError.Package(
          facts.run,
          "closure",
          "Manifest entries and delivered artifact names differ."
        )
      )
      _ <- Either.cond(
        Set(PlanName, InputName, LedgerName, ResultName).subsetOf(
          graph.entries.map(_.name).toSet
        ),
        (),
        NativeArtifactError.Package(
          facts.run,
          "closure",
          "Required plan/input/ledger/result roles are missing."
        )
      )
      _ <- Either.cond(
        graph.entries.forall(e =>
          e.name match
            case `PlanName`   => e.role == ArtifactRole.StudyPlan
            case `InputName`  => e.role == ArtifactRole.StudyInput
            case `LedgerName` => e.role == ArtifactRole.AdmissionLedger
            case `ResultName` => e.role == ArtifactRole.StudyResult
            case _            => e.role == ArtifactRole.ResultPayload
        ),
        (),
        NativeArtifactError.Package(
          facts.run,
          "closure roles",
          "Only the named plan/input/ledger/result and their density payloads are admitted."
        )
      )
      required = Set[ManifestRelation](
        ManifestRelation.PlanInput(PlanName, InputName),
        ManifestRelation.LedgerOf(LedgerName, InputName),
        ManifestRelation.ResultOf(ResultName, PlanName, InputName)
      )
      _ <- Either.cond(
        required.subsetOf(graph.relations.toSet) && graph.relations.forall {
          case ManifestRelation.ResultPayloadOf(owner, _) => owner == ResultName
          case edge                                       => required(edge)
        },
        (),
        NativeArtifactError.Package(
          facts.run,
          "closure relations",
          "Canonical facts require this result's own plan, input and ledger."
        )
      )
      _ <- limit(
        "density cells",
        graph.entries.flatMap(_.layout).foldLeft(BigInt(0))((n, layout) => n + layout.count),
        BigInt(budget.maxDensityCells)
      )
      structuralRows <- graph.entries
        .filter(_.media == MediaKind.JsonText)
        .foldLeft[Either[NativeArtifactError, BigInt]](Right(BigInt(0))) { (total, entry) =>
          total.flatMap(n =>
            need(entry.name)
              .flatMap(decoded(entry.name.value, _))
              .map(doc => n + fieldRows(doc))
          )
        }
      _ <- limit(
        "structural rows",
        structuralRows,
        BigInt(budget.maxRows)
      )
      address = ByteDigest.sha256(NativeArtifactBytes.view(manifestBytes))
      source  = ByteSource.inMemory(
        Map(address -> NativeArtifactBytes.view(manifestBytes)),
        graph.entries.map(e => e.name -> NativeArtifactBytes.view(byName(e.name))).toMap
      )
      decoders <- layout("native decoders")(ArtifactDecoders.trial[Px])
      resolved <- ArtifactResolver
        .resolveOwned(address, source, decoders)
        .leftMap(errors =>
          NativeArtifactError
            .Package(facts.run, "resolve", errors.toVector.map(_.message).mkString("; "))
        )
      plan <- resolved
        .plan(PlanName)
        .toRight(NativeArtifactError.Package(facts.run, "plan", "Missing decoded plan."))
      input <- resolved
        .input(InputName)
        .toRight(NativeArtifactError.Package(facts.run, "input", "Missing decoded input."))
      ledger <- resolved
        .ledger(LedgerName)
        .toRight(NativeArtifactError.Package(facts.run, "ledger", "Missing decoded ledger."))
      result <- resolved
        .result(ResultName)
        .toRight(NativeArtifactError.Package(facts.run, "result", "Missing decoded result."))
      frame <- input.trials.rows.headOption
        .map(_.value.frame)
        .toRight(
          NativeArtifactError.Package(
            facts.run,
            "recipe verification",
            "Empty input has no native source frame."
          )
        )
      expected <- RealPlan
        .plan(facts.revision, facts.recipeSnapshot, frame, input)
        .leftMap(e => NativeArtifactError.InvalidFacts(facts.run, "recipe", e.message))
      nativePlans   = StudyCodecs.trialSimilarity[Px](expected._2)
      nativeResults = StudyResultCodecs.trialRegistered[Px](expected._2)
      planJson   <- layout("decoded plan encode")(plan.encode)
      nativePlan <- layout("native plan decode")(nativePlans.codec.decode(planJson))
      planDigest <- layout("decoded plan canonical digest")(
        nativePlans.codec.digest(nativePlan)
      )
      inputDigest <- layout("decoded input canonical digest")(
        StudyInputCodecs.trial[Px].input.digest(input)
      )
      _ <- Either.cond(
        result.stampClaim.isEmpty,
        (),
        NativeArtifactError.Package(
          facts.run,
          "result",
          "This native package exports plain retained results, not stamped archives."
        )
      )
      resultPayload <- layout("stored result metadata")(result.reconstructionMetadata)
      nativeResult  <- layout("native result reconstruction")(
        nativeResults.restoreDensities(
          resultPayload,
          result.result.scales.map(_.estimation.map(_._2.toOption))
        )
      )
      resultDigest <- layout("decoded result canonical digest")(
        nativeResults.codec.digest(nativeResult)
      )
      semantic <- SemanticIdentity
        .of(ledger.source.records.digest)
        .leftMap(e => NativeArtifactError.InvalidFacts(facts.run, "source", e.message))
      _ <- Either.cond(
        planDigest.sha256 == facts.planCanonical.sha256 && inputDigest.sha256 == facts.inputCanonical.sha256 &&
          resultDigest.sha256 == facts.result.sha256 && semantic == facts.source,
        (),
        NativeArtifactError.InvalidFacts(
          facts.run,
          "canonical identities",
          s"Declared plan ${facts.planCanonical.display}, decoded ${planDigest.display}; " +
            s"declared input ${facts.inputCanonical.display}, decoded ${inputDigest.display}; " +
            s"declared result ${facts.result.display}, decoded ${resultDigest.display}; " +
            s"declared source ${facts.source.value}, decoded ${semantic.value}."
        )
      )
      expectedDigest <- layout("recipe plan canonical digest")(
        nativePlans.codec.digest(expected._1)
      )
      _ <- Either.cond(
        expectedDigest.sha256 == facts.planCanonical.sha256,
        (),
        NativeArtifactError.InvalidFacts(
          facts.run,
          "recipe",
          s"Recipe snapshot configures plan ${expectedDigest.display}; decoded plan is ${planDigest.display}."
        )
      )
      archive <- RunArchive
        .of(
          RunRef(
            facts.run,
            facts.revision,
            facts.dataset,
            RunLifecycle.Completed,
            CoreBinding.Bound(facts.result)
          ),
          owned.map((name, data) => name -> NativeArtifactBytes.view(data))
        )
        .leftMap(e => NativeArtifactError.Package(facts.run, "archive", e.message))
    yield new NativeArtifactPackage(
      facts,
      graph,
      address,
      budget,
      owned,
      archive,
      new CheckedNativeSnapshot(nativePlan, expected._2, input, ledger, nativeResult)
    )
