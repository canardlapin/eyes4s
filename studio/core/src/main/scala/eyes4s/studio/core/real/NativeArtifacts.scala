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

package eyes4s.studio.core.real

import cats.syntax.all.*
import eyes4s.codec.*
import eyes4s.plan.StudyDesign
import eyes4s.studio.core.artifacts.*
import eyes4s.studio.core.backend.RunId
import eyes4s.studio.core.document.*
import eyes4s.studio.core.execution.{RunStamp, StudyInputArtifact}
import java.nio.charset.StandardCharsets.UTF_8

/** Export only a retained completed value, without another execution or an
  * invented stamp; bead q-native-stored-completion records the binding seam.
  */
private[real] object NativeArtifacts:
  private enum Storage:
    case LegacyJson, Columnar

  def build(
      run: RunId,
      held: RealStudyBackend.RealRun,
      budget: NativeArtifactBudget
  ): Either[NativeArtifactError, NativeArtifactPackage] =
    buildIn(run, held, budget, Storage.LegacyJson).left.flatMap {
      case NativeArtifactError.Budget(_, resource, _, _)
          if Set("structural rows", "encoded archive lower bound")(resource) =>
        buildIn(run, held, budget, Storage.Columnar)
      case NativeArtifactError.UnscopedBudget(resource, _, _)
          if Set("structural rows", "total bytes")(resource) =>
        buildIn(run, held, budget, Storage.Columnar)
      case other => Left(other)
    }

  private def buildIn(
      run: RunId,
      held: RealStudyBackend.RealRun,
      budget: NativeArtifactBudget,
      storage: Storage
  ): Either[NativeArtifactError, NativeArtifactPackage] =
    val work       = held.prepared
    val result     = held.result
    val successful =
      result.scales.flatMap(_.estimation).collect { case (key, Right(mass)) => key -> mass }
    val cells   = successful.foldLeft(BigInt(0))((n, value) => n + value._2.values.length)
    val entries = BigInt(successful.map(_._1.participant).distinct.size) + 6
    val rows    = BigInt(work.admitted.evidence.records.size) + work.admitted.input.trials.rows
      .foldLeft(BigInt(0))((n, trial) => n + trial.value.n + 1) + result.scales.foldLeft(
      BigInt(0)
    ) { (n, scale) =>
      n + scale.estimation.size + scale.analyses.source(StudyDesign.Matched).rows.size +
        scale.analyses.source(StudyDesign.Control).rows.size
    }
    def limit(resource: String, value: BigInt, maximum: BigInt) =
      Either.cond(
        value <= maximum,
        (),
        NativeArtifactError.Budget(run, resource, value, maximum)
      )
    def codec[A](operation: String)(value: Either[CodecError, A]) =
      value.leftMap(e => NativeArtifactError.Package(run, operation, e.message))
    for
      _ <- Either.cond(
        work.admitted.input.trials.nonEmpty,
        (),
        NativeArtifactError.Package(
          run,
          "recipe verification",
          "Empty admitted input has no native source frame."
        )
      )
      _ <- limit("density cells", cells, BigInt(budget.maxDensityCells))
      _ <-
        if storage == Storage.LegacyJson then
          limit("structural rows", rows, BigInt(budget.maxRows))
        else Right(())
      _ <- limit("entries", entries, BigInt(budget.maxEntries))
      // Packed cells alone establish this lower bound before encoding JSON or allocating chunks.
      _       <- limit("packed density bytes", cells * 8, BigInt(budget.maxTotalBytes))
      compact <-
        if storage == Storage.Columnar then
          codec("compact limits")(
            CompactArtifactLimits.of(budget.maxRows, budget.maxTotalBytes)
          ).map(Some(_))
        else Right(None)
      p <- codec("plan artifact")(StoredArtifact.plan("plan", work.plans, work.plan))
      i <- codec("input artifact")(
        compact.fold(StoredArtifact.input("input", work.inputs, work.admitted.input))(limits =>
          StoredArtifact.compactInput("input", work.inputs, work.admitted.input, limits)
        )
      )
      l <- codec("ledger artifact")(
        compact.fold(StoredArtifact.ledger("ledger", work.inputs, work.admitted.evidence))(
          limits =>
            StoredArtifact.compactLedger("ledger", work.inputs, work.admitted.evidence, limits)
        )
      )
      // These exact retained bytes plus packed cells are a lower bound on the
      // final package. Refuse before result hashing/chunk construction when it
      // already exceeds the unchanged whole-package limit.
      _ <- limit(
        "encoded archive lower bound",
        BigInt(p.bytes.length) + i.bytes.length + l.bytes.length + cells * 8,
        BigInt(budget.maxTotalBytes)
      )
      planDigest   <- codec("plan digest")(work.plans.codec.digest(work.plan))
      inputDigest  <- codec("input digest")(work.inputs.input.digest(work.admitted.input))
      resultDigest <- codec("result digest")(work.results.codec.digest(result))
      plan         <- CanonicalDigest
        .parse[StudyPlanArtifact](planDigest.sha256.hex)
        .leftMap(e => NativeArtifactError.Package(run, "plan binding", e.message))
      input <- CanonicalDigest
        .parse[StudyInputArtifact](inputDigest.sha256.hex)
        .leftMap(e => NativeArtifactError.Package(run, "input binding", e.message))
      resultCanonical <- CanonicalDigest
        .parse[ResultArchiveArtifact](resultDigest.sha256.hex)
        .leftMap(e => NativeArtifactError.Package(run, "result binding", e.message))
      source <- SemanticIdentity
        .of(work.admitted.evidence.source.records.digest)
        .leftMap(e => NativeArtifactError.InvalidFacts(run, "semantic source", e.message))
      definition <- codec("dataset definition")(NativeDatasetDefinition.of(work.admitted.spec))
      facts      <- NativeBindingFacts.of(
        run,
        RunStamp(
          work.revision,
          work.dataset,
          CoreBinding.Bound(plan),
          CoreBinding.Bound(input)
        ),
        source,
        resultCanonical,
        work.recipe,
        Some(definition)
      )
      packed <- codec("packed result artifacts")(
        compact.fold(ResultManifest.packed("result", work.results, result))(limits =>
          ResultManifest.compactPacked("result", work.results, result, limits)
        )
      )
      saved <- codec("manifest")(
        SavedManifest.of(
          Vector(p, i, l, packed.result) ++ packed.payloads,
          Vector(
            ManifestRelation.PlanInput(p.name, i.name),
            ManifestRelation.LedgerOf(l.name, i.name),
            ManifestRelation.ResultOf(packed.result.name, p.name, i.name)
          ) ++ packed.relations
        )
      )
      factCodec <- codec("facts codec")(NativeBindingFacts.codec)
      factJson  <- codec("facts encode")(factCodec.encode(facts))
      files = Vector(
        NativeArtifactPackage.FactsName -> NativeArtifactBytes.copyOf(
          IArray.from(factJson.noSpaces.getBytes(UTF_8))
        ),
        NativeArtifactPackage.ManifestName -> NativeArtifactBytes.copyOf(saved.bytes)
      ) ++ saved.artifacts.map(a => a.name -> NativeArtifactBytes.copyOf(a.bytes))
      verified <- NativeArtifactPackage.verify(files, budget)
      _        <- Either.cond(
        verified.facts == facts,
        (),
        NativeArtifactError
          .InvalidFacts(run, "verified facts", "Resolver returned different facts.")
      )
    yield verified
