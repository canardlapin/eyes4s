# CR4: every analysis family meets the UI definition of done

Ticket: `bd-01M3DH0RZP5ZV2K70PD0ZVS7PK` (absorbs `pl-analysis`). It blocks CR6
(`bd-01M3DH0S5ZKFCN47MHDM0XWW10`). Parent: `docs/plans/CORE_READINESS_PLAN.md`.
The inventory was taken against the CR2 branch at `b030d63`.

## Reference: how the study family meets the contract

| Contract | Study family |
|---|---|
| Codec and built-in schema | `StudyCodecs.similarity`, `StudyCodecDefinitions`; `ArtifactResolution.study` registers every `ComparisonMethods.all` entry |
| RecipeFamily | `Preflight.scala` `RecipeFamily.FixationStudy`; `StudyReport.family` |
| Archive role | `ArtifactRole.StudyPlan`/`StudyResult` with the `ResultOf` relation (`ScientificManifest.scala`) |
| Descriptor and inspect | `StudyMethod.descriptor`, `ComparisonMethod.descriptor`, `StudyPlan.inspect`/`description`/`diff` via `RecipeDescriptors.study` |
| Preflight and diagnostics | `Preflight.study`, `StudyFinding`, `Diagnose.studyFinding`/`plan`/`studyFailure`, catalog `studyFinding`/`preflight` |
| Stepwise | `StudyCursor` with `given Stepwise.study` |
| fs2 route | `StudyExecution.submission` through the shared `Execution`/`Submission`; `ExecutionLawsSuite` |
| Result table | `BaselineExports` over `ResultTable` (moving to `eyes4s-results` with UI-C) |

Recording and temporal already meet the contract. Map methods inside `template_similarity` were done
by CR2; the trial-keyed route remains cosine only.

## Gaps

| Family | Missing |
|---|---|
| Repetition | built-in schemas, RecipeFamily, archive role, descriptor/inspect, preflight, cursor + fs2 route (the `run` is eager), result codec |
| Point sampling | built-in schemas, RecipeFamily, role, descriptor, preflight, cursor, K-keyed error operands |
| Template fit/CV | built-in schema (it exists only in a test), RecipeFamily, role, descriptor, cursor |
| Decomposition | plan, codec, family, role, descriptor, cursor |
| Scanpath/fixation comparison | plan, method registry, codec, family, role, descriptor, cursor, table |
| Epochs | descriptor and built-in schema only (they are a component) |

## Decisions (lead, 2026-09-26)

- **Epochs are a component, not a family.** They get a descriptor, a built-in schema and the
  existing `Diagnose`. The conformance suite lists them as a component, with no cursor and no
  archive role.
- **Archive roles are generic.** CR4 adds `analysis-plan`/`analysis-result` roles and an
  `AnalysisResultOf(result, plan, inputs)` relation, dispatched by schema through an
  `AnalysisRegistry`. It does not add one role per family. CR4 therefore contributes once to the
  single manifest@2 (after UI-C's report roles, SourceRef's source roles and UI-E's
  `ResultPayload`), and later families need no manifest bump. The three existing families keep
  their dedicated roles.
- **New result schemas carry UI-D's `RunStamp` from their first version**, so they need no later
  version bump.
- New `DefinitionId`s go in a `*Definitions` object in the file that introduces them, with a
  registry entry, a law and a pinned fixture (`AGENTS.md`, shared registries).

## Shared scaffolding (lands in S1)

- **Findings.** `PreflightFinding` and `PreflightReport` are sealed, so the generic types live in
  `Preflight.scala`: `enum AnalysisFinding[K]` (`MissingArtifact`, `ArtifactMismatch`,
  `Refused(Diagnostic[K])`, `DataDependent(Diagnostic[K], keys)`) and `AnalysisReport[K, F]` with
  `confirm`. Later families add no finding types there.
- **Diagnostics.** A catalog family `analysis-finding`, and a `given Diagnose[AnalysisFinding[K], K]`.
- **Where each family's code goes.**
  - Stepwise instances live in each cursor's companion, so `BoundedWork.scala` is not edited again.
  - Each family gets its own file in plan (`RepetitionFamily.scala`, `PointSamplingFamily.scala`,
    `TemplateFamily.scala`, `ScanpathStudy.scala`) holding `description`, `diff`, `inspect`,
    `preflight`, the cursor and the stage enum.
  - The `RecipeDescriptors` helpers stay public (narrowing them would be a MiMa break with no benefit); family files call them directly.
- **Conformance.** `FamilyConformanceSuite` in `io` tests (`io` sees plan, codec, fs2, results and
  laws).
  - An exhaustive match over `RecipeFamily.values` into a witness means a new family fails
    `-Werror` compilation until it has one.
  - Each witness checks five things:
    - inspect explains every field;
    - the preflight family matches and its diagnostics are catalogued;
    - `Stepwise.complete` equals `run`;
    - `Execution` completes and cancels between steps;
    - the codec/archive round trip and the table projection both hold.
  - A plan-side `AnalysisKind` catalog maps every analysis to `Option[RecipeFamily]`. The analyses
    mapped to `None` must equal an explicit allowlist, and each slice shrinks it.
  - Laws go in one new suite per family.

## Slices, ranked by Studio value

| Slice | Content | Acceptance | Size |
|---|---|---|---|
| S1 | Harness and map route: `AnalysisKind` and its allowlist, `FamilyConformanceSuite` over the three current families, `AnalysisFinding`/`AnalysisReport`, `analysis-finding` catalog codes; `StudyCodecs.trialSimilarity(method)` and every method registered for `TrialKey` | Suite green; removing a witness fails compilation; every `ComparisonMethods.all` entry round-trips on both key routes | S–M |
| S2 | Generic `analysis-plan`/`analysis-result` roles, `AnalysisResultOf`, `AnalysisRegistry`, manifest laws and fixtures, in the single manifest@2 | manifest@1 archives still decode; an unknown analysis schema is a typed `CodecError` | M |
| S3 | Repetition family: RecipeFamily case, descriptor, preflight, `RepetitionCursor` (Matched/Control stages on `EvaluationWork`), `fs2/RepetitionExecution`, result codec, table in `results` | Cursor equals `run`; eyesim-repetition parity unchanged; a cursor mutant is killed | M |
| S4 | Template fit/CV (`TemplateFit` family: Fitting then Evaluating) and `DecompositionPlan` with codec and cursor; built-in template schemas | Refit from a saved recipe is deterministic; eyesim-template parity holds | M–L |
| S5 | Point sampling family and the epoch component descriptor/schema; K-keyed `PointSamplingError` | eyesim-sampling parity holds; preflight names trials by key | M |
| S6 | Scanpath/fixation comparison: `ScanpathStudyPlan` reusing `StudyLayout`/`StudyPairing`/`StudyInput`, a `ScanpathMethods` registry (MultiMatch, overlap, transport) with typed parameters, cursor, fs2 route, codec, table | eyesim-scanpath parity holds; work bounds coordinated with CR8 | L |

## Order and conflicts

1. **S1**, after CR2 lands. Its `DiagnosticCatalog`/`Preflight` hunks rebase over UI-E and UI-D,
   which also edit those files.
2. **UI-C** lands; the family result tables move into `eyes4s-results` after it.
3. **The single manifest@2.** The lead lands it: UI-C roles, SourceRef roles, UI-E `ResultPayload`,
   CR4 S2 roles.
4. **Family slices S3–S6** after UI-D, because their submissions carry UI-D's required `StageMeter`.

Files with in-flight overlap:
- `codec/ScientificManifest.scala`, `ArtifactResolution.scala`: UI-C, UI-E, SourceRef.
- `plan/DiagnosticCatalog.scala`, `Projections.scala`: UI-E.
- `plan/Preflight.scala`, `fs2/Execution.scala`, `laws/ExecutionLaws.scala`: UI-D.
- `io/BaselineExports.scala`, `ResultTable.scala`, `build.sbt`: UI-C.
- `SchemaRegistryJvmSuite`: append-only.
