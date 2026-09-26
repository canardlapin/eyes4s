# Packed result payloads in a manifest

`ResultManifest.packed(name, resultCodec, result)` writes a study result using
UI-E's packed density policy: one float64 little-endian chunk per participant,
across scales. It returns `ResultArtifacts` containing the result document,
its payload artifacts, and their `ResultPayloadOf(result, payload)` relations.
Chunk names use deterministic indices rather than participant identifiers.

Add the saved plan and input artifacts, a `PlanInput` relation and a `ResultOf`
relation, then pass all artifacts and relations to `SavedManifest.of`. Storage
still belongs to the caller; the codec layer performs no file access.

The new `result-payload` role and `result-payload-of` relation are additive within
`eyes4s.manifest@1`. They use the existing `eyes4s.packed-array@1` schema and
`PayloadLayout`, and the owner uses `eyes4s.study-result@2`. Existing manifest@1
fixtures retain their exact bytes and addresses. Unknown roles and relation kinds
produce typed codec refusals.

The ordinary `ArtifactDecoders.of` / `ArtifactDecoders.study` registrations and
`ArtifactResolver` reconstruct packed results. Resolution verifies byte lengths
and SHA-256 digests before scientific decoding, gives a result only the verified
chunks named by its own relations, checks each map's saved geometry, provenance
and density digest, and applies the existing completed-result reconstruction
checks. Every result payload needs an owner, and each relation must point to a
chunk actually referenced by that result. Recording `Payload` / `PayloadOf`
semantics remain unchanged.

`ArtifactDecoders.Delegating` forwards the packed-result decoder. A custom
`ArtifactDecoders` implementation can override `resultWithPayloads`; its default
uses the existing result decoder and preserves its refusal for unsupported
archives. The loaded value is a materialized result, and its ordinary `encode`
method writes the existing inline result representation. Resolution is eager;
this slice makes no lazy-file-access or bounded-execution claim.

Recomputable density archives carry no packed payloads. Their plan/input-bound
recomputation API remains `DensityArchiveCodec.recomputer`; the manifest resolver
introduced here does not execute that computation automatically. A recomputable
archive supplied to this resolver is refused with a density decode error rather
than treated as a completed result.
