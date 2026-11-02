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
import eyes4s.io.{LocationBundleImport, LocationBundleText, LocationImport}
import eyes4s.plan.{
  DefinitionId,
  LocationComparisonPlan,
  LocationComparisonResult,
  LocationInput
}
import eyes4s.studio.core.backend.RunId
import eyes4s.studio.core.document.*
import eyes4s.studio.core.document.DigestJson.given
import eyes4s.studio.core.runs.RunArchive
import io.circe.{Codec, Decoder, DecodingFailure, Encoder}
import io.circe.syntax.*
import java.nio.charset.StandardCharsets.UTF_8

/** Parsed sources and their canonical spatial input, without evaluating a map or score. */
final class AdmittedLocations private[artifacts] (
    val dataset: LocationDatasetRevision,
    val imported: LocationImport,
    val plan: LocationComparisonPlan,
    val inputCanonical: CanonicalDigest[LocationInputArtifact],
    val planCanonical: CanonicalDigest[LocationPlanArtifact],
    val sourceFiles: Vector[(SourceRole, IndexedSeq[Byte])]
)

/** Spatial completion facts cannot contain unbound input or plan identities. */
sealed abstract case class LocationBindingFacts private (
    run: RunId,
    dataset: LocationDatasetRevision,
    analysis: LocationAnalysisRevision,
    result: CanonicalDigest[ResultArchiveArtifact]
) derives CanEqual

object LocationBindingFacts:
  def of(
      run: RunId,
      dataset: LocationDatasetRevision,
      analysis: LocationAnalysisRevision,
      result: CanonicalDigest[ResultArchiveArtifact]
  ): Either[NativeArtifactError, LocationBindingFacts] =
    val agrees = (dataset.decision, analysis.recipe.input, analysis.plan) match
      case (
            LocationAdmissionDecision.Admitted(input),
            CoreBinding.Bound(bound),
            CoreBinding.Bound(_)
          ) =>
        input == bound
      case _ => false
    Either.cond(
      run.number > 0 && dataset.id.number > 0 && analysis.id.number > 0 && analysis.dataset == dataset.id && analysis.recipe.profile == dataset.profile && agrees,
      new LocationBindingFacts(run, dataset, analysis, result) {},
      NativeArtifactError.InvalidFacts(
        run,
        "spatial identities",
        s"${analysis.id.label}/${analysis.dataset.label} must bind the admitted input and profile of ${dataset.id.label}, and a canonical location plan."
      )
    )

  private case class Wire(
      run: RunId,
      dataset: LocationDatasetRevision,
      analysis: LocationAnalysisRevision,
      result: CanonicalDigest[ResultArchiveArtifact]
  ) derives Codec.AsObject
  given Encoder.AsObject[LocationBindingFacts] = Encoder.AsObject.instance(f =>
    summon[Encoder.AsObject[Wire]].encodeObject(Wire(f.run, f.dataset, f.analysis, f.result))
  )
  given Decoder[LocationBindingFacts] = Decoder.instance(c =>
    c.as[Wire]
      .flatMap(w =>
        of(w.run, w.dataset, w.analysis, w.result)
          .leftMap(e => DecodingFailure(e.message, c.history))
      )
  )
  val codec: Either[CodecError, VersionedCodec[LocationBindingFacts]] =
    DefinitionId.of("studio.location-binding-facts", 1).leftMap(CodecError.Definition(_)).map {
      schema =>
        VersionedCodec
          .checked[LocationBindingFacts](schema)(f => Right(CanonicalJson(f.asJson)))(json =>
            json
              .as[LocationBindingFacts]
              .leftMap(e => CodecError.Field("spatial binding facts", json, e.message))
          )
    }

/** A closed spatial run: exact five-file custody, checked metadata and stored scores.
  * Cold verification re-admits archived bytes and configures a plan; it never runs it.
  */
final class LocationNativePackage private (
    val facts: LocationBindingFacts,
    val admitted: AdmittedLocations,
    val retained: LocationComparisonRun,
    val budget: NativeArtifactBudget,
    val files: Vector[(ArtifactName, IndexedSeq[Byte])],
    val archive: RunArchive
)

object LocationNativePackage:
  val FactsName: ArtifactName  = ArtifactName.of("studio.location-binding-facts").toOption.get
  val ResultName: ArtifactName = ArtifactName.of("location-result").toOption.get
  val sourceNames: Vector[(SourceRole, ArtifactName)] = Vector(
    SourceRole.LocationTrials -> "location-trials",
    SourceRole.Locations      -> "locations",
    SourceRole.LocationPairs  -> "location-pairs",
    SourceRole.PhaseLedger    -> "phase-ledger",
    SourceRole.LocationLedger -> "location-ledger"
  ).map((role, name) => role -> ArtifactName.of(name).toOption.get)

  val inputCodec: Either[CodecError, VersionedCodec[LocationInput]] =
    DefinitionId
      .of("studio.location-input", 1)
      .leftMap(CodecError.Definition(_))
      .map(LocationInputCodec.of)

  private def layout[A](operation: String)(
      value: Either[CodecError, A]
  ): Either[NativeArtifactError, A] =
    value.leftMap(e => NativeArtifactError.Layout(operation, e.message))
  private def typed[A, B](
      digest: CanonicalDigest[A]
  ): Either[NativeArtifactError, CanonicalDigest[B]] =
    CanonicalDigest
      .parse[B](digest.sha256.hex)
      .leftMap(e => NativeArtifactError.Layout("canonical digest", e.message))
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
  private def text(name: String, bytes: IndexedSeq[Byte]): Either[NativeArtifactError, String] =
    val decoded = String(bytes.toArray, UTF_8)
    val encoded = decoded.getBytes(UTF_8)
    Either.cond(
      encoded.length == bytes.size && encoded.indices.forall(i => encoded(i) == bytes(i)),
      decoded,
      NativeArtifactError.Layout(name, "Invalid UTF-8 bytes.")
    )
  private def bytes(value: String): IndexedSeq[Byte] =
    NativeArtifactBytes.copyOf(value.getBytes(UTF_8).toIndexedSeq)

  def admit(
      dataset: LocationDatasetRevision,
      files: Vector[(SourceRole, IndexedSeq[Byte])],
      budget: NativeArtifactBudget = NativeArtifactBudget.Default
  ): Either[NativeArtifactError, AdmittedLocations] =
    val roles = files.map(_._1)
    for
      _ <- limit(
        "source bytes",
        files.foldLeft(BigInt(0))((total, file) => total + file._2.size),
        BigInt(budget.maxTotalBytes)
      )
      _ <- Either.cond(
        roles.distinct.size == roles.size && roles.toSet == LocationBundleSources.roles.toSet,
        (),
        NativeArtifactError.Layout(
          "spatial source roles",
          s"Expected ${LocationBundleSources.roles}; found $roles."
        )
      )
      owned = files.map((role, data) => role -> NativeArtifactBytes.copyOf(data)).toMap
      texts <- dataset.sources.entries.traverse { source =>
        val data   = owned(source.role)
        val digest = ByteDigest.sha256(NativeArtifactBytes.view(data))
        for
          _ <- Either.cond(
            digest == source.bytes,
            (),
            NativeArtifactError.Layout(
              source.path.value,
              s"Expected ${source.bytes.hex}; found ${digest.hex}."
            )
          )
          decoded <- text(source.path.value, data)
        yield source.role -> decoded
      }
      byRole = texts.toMap
      _ <- limit(
        "source lines",
        texts.foldLeft(BigInt(0))((total, file) => total + file._2.count(_ == '\n')),
        BigInt(budget.maxRows)
      )
      imported <- LocationBundleImport
        .read(
          LocationBundleText(
            byRole(SourceRole.LocationTrials),
            byRole(SourceRole.Locations),
            byRole(SourceRole.LocationPairs),
            byRole(SourceRole.PhaseLedger),
            byRole(SourceRole.LocationLedger)
          )
        )
        .leftMap(e => NativeArtifactError.Layout("spatial admission", e.message))
      _ <- limit(
        "density cells",
        BigInt(imported.input.trials.size) * imported.profile.columns * imported.profile.rows,
        BigInt(budget.maxDensityCells)
      )
      plan <- LocationComparisonPlan
        .binned(
          imported.input,
          imported.profile.columns,
          imported.profile.rows,
          imported.queries
        )
        .leftMap(e => NativeArtifactError.Layout("spatial plan", e.message))
      inputs      <- layout("spatial input codec")(inputCodec)
      inputDigest <- layout("spatial input digest")(inputs.digest(imported.input))
        .flatMap(typed[LocationInput, LocationInputArtifact])
      planDigest <- layout("spatial plan digest")(
        LocationComparisonResultCodec.plans.digest(plan)
      ).flatMap(typed[LocationComparisonPlan, LocationPlanArtifact])
      _ <- dataset.decision match
        case LocationAdmissionDecision.Pending            => Right(())
        case LocationAdmissionDecision.Admitted(expected) =>
          Either.cond(
            expected == inputDigest,
            (),
            NativeArtifactError.Layout(
              "spatial admitted input",
              s"${dataset.id.label} binds ${expected.display}; decoded ${inputDigest.display}."
            )
          )
    yield new AdmittedLocations(
      dataset.copy(decision = LocationAdmissionDecision.Admitted(inputDigest)),
      imported,
      plan,
      inputDigest,
      planDigest,
      sourceNames.map((role, _) => role -> owned(role))
    )

  private def agrees(
      facts: LocationBindingFacts,
      admitted: AdmittedLocations
  ): Either[NativeArtifactError, Unit] =
    Either.cond(
      facts.dataset == admitted.dataset && facts.analysis.plan == CoreBinding.Bound(
        admitted.planCanonical
      ) && facts.analysis.recipe.input == CoreBinding.Bound(admitted.inputCanonical),
      (),
      NativeArtifactError.InvalidFacts(
        facts.run,
        "spatial source closure",
        "Stored dataset, input and plan identities do not agree with the five archived sources."
      )
    )

  def create(
      run: RunId,
      admitted: AdmittedLocations,
      analysis: LocationAnalysisRevision,
      result: LocationComparisonResult,
      budget: NativeArtifactBudget = NativeArtifactBudget.Default
  ): Either[NativeArtifactError, LocationNativePackage] =
    val retained = LocationComparisonRun(admitted.plan, result)
    for
      _ <- limit("entries", BigInt(7), BigInt(budget.maxEntries))
      _ <- limit(
        "density cells",
        BigInt(admitted.plan.input.trials.size) * admitted.plan.grid.size,
        BigInt(budget.maxDensityCells)
      )
      _ <- limit(
        "source bytes",
        admitted.sourceFiles.foldLeft(BigInt(0))((total, file) => total + file._2.size),
        BigInt(budget.maxTotalBytes)
      )
      _ <- limit(
        "source lines",
        admitted.sourceFiles.foldLeft(BigInt(0))((total, file) =>
          total + file._2.count(_ == 10.toByte)
        ),
        BigInt(budget.maxRows)
      )
      resultJson <- layout("spatial result")(
        LocationComparisonResultCodec.codec.encode(retained)
      )
      resultDigest <- layout("spatial result digest")(
        LocationComparisonResultCodec.codec.digest(retained)
      ).flatMap(typed[LocationComparisonRun, ResultArchiveArtifact])
      facts      <- LocationBindingFacts.of(run, admitted.dataset, analysis, resultDigest)
      _          <- agrees(facts, admitted)
      factsCodec <- layout("spatial facts codec")(LocationBindingFacts.codec)
      factsJson  <- layout("spatial facts")(factsCodec.encode(facts))
      sources = admitted.sourceFiles.toMap
      files   = Vector(
        FactsName  -> bytes(CanonicalJson(factsJson).noSpaces),
        ResultName -> bytes(CanonicalJson(resultJson).noSpaces)
      ) ++ sourceNames.map((role, name) => name -> sources(role))
      packaged <- complete(facts, admitted, retained, files, budget)
    yield packaged

  def verify(
      files: Vector[(ArtifactName, IndexedSeq[Byte])],
      budget: NativeArtifactBudget = NativeArtifactBudget.Default
  ): Either[NativeArtifactError, LocationNativePackage] =
    val expected = Set(FactsName, ResultName) ++ sourceNames.map(_._2)
    for
      _ <- limit("entries", BigInt(files.size), BigInt(budget.maxEntries))
      _ <- limit(
        "total bytes",
        files.foldLeft(BigInt(0))((total, file) => total + file._2.size),
        BigInt(budget.maxTotalBytes)
      )
      _ <- Either.cond(
        files.map(_._1).toSet == expected && files.size == expected.size,
        (),
        NativeArtifactError.Layout(
          "spatial closure",
          "Exactly the binding facts, retained result and five source artifacts are required."
        )
      )
      owned  = files.map((name, data) => name -> NativeArtifactBytes.copyOf(data))
      byName = owned.toMap
      factsText  <- text(FactsName.value, byName(FactsName))
      factsCodec <- layout("spatial facts codec")(LocationBindingFacts.codec)
      facts      <- layout("spatial facts")(factsCodec.parse(factsText))
      admitted   <- admit(
        facts.dataset,
        sourceNames.map((role, name) => role -> byName(name)),
        budget
      )
      _          <- agrees(facts, admitted)
      resultText <- text(ResultName.value, byName(ResultName))
      retained   <- layout("spatial retained result")(
        LocationComparisonResultCodec.codec.parse(resultText)
      )
      digest <- layout("spatial retained digest")(
        LocationComparisonResultCodec.codec.digest(retained)
      )
      planDigest <- layout("spatial retained plan digest")(
        LocationComparisonResultCodec.plans.digest(retained.plan)
      )
      _ <- Either.cond(
        digest.sha256 == facts.result.sha256 && planDigest.sha256 == admitted.planCanonical.sha256,
        (),
        NativeArtifactError.InvalidFacts(
          facts.run,
          "spatial retained identities",
          "The retained plan/result disagree with the archived source closure and binding facts."
        )
      )
      packaged <- complete(facts, admitted, retained, owned, budget)
    yield packaged

  private def complete(
      facts: LocationBindingFacts,
      admitted: AdmittedLocations,
      retained: LocationComparisonRun,
      files: Vector[(ArtifactName, IndexedSeq[Byte])],
      budget: NativeArtifactBudget
  ): Either[NativeArtifactError, LocationNativePackage] =
    for
      _ <- limit("entries", BigInt(files.size), BigInt(budget.maxEntries))
      _ <- limit(
        "total bytes",
        files.foldLeft(BigInt(0))((total, file) => total + file._2.size),
        BigInt(budget.maxTotalBytes)
      )
      _ <- limit(
        "density cells",
        BigInt(retained.result.maps.size) * retained.plan.grid.size,
        BigInt(budget.maxDensityCells)
      )
      archive <- RunArchive
        .of(
          RunRef(
            facts.run,
            facts.analysis.id,
            facts.dataset.id,
            RunLifecycle.Completed,
            CoreBinding.Bound(facts.result)
          ),
          files.map((name, data) => name -> NativeArtifactBytes.view(data))
        )
        .leftMap(e => NativeArtifactError.Package(facts.run, "spatial archive", e.message))
    yield new LocationNativePackage(facts, admitted, retained, budget, files, archive)
