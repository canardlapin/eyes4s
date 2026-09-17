# eyes4s-codec

Versioned JSON codecs and typed persistence registration for fixation-study plans.
`VersionedCodec[A]` returns typed failures on encode/decode. `StudyCodec` captures the method,
key layout, key codec and parameter codec before registration. Generic maps encode as entry arrays
and reject duplicate keys. Inputs are separate typed content references, not embedded numeric blobs.
`StudyInputCodec` serializes the fixation-study input itself and its admission ledger as separate
payloads: typed keys through a registered key codec, frames and clocks through the document identity
table, decimal-string microseconds, a digest check on decode, and one typed disposition per source
record. `VersionedCodec.trials` is the generic row-array codec for `Trials[K, M, A]`.

Start with [saved studies](../docs/SAVED_STUDIES.md) or the
[extension guide](../docs/EXTENDING_STUDIES.md). Codecs for the rest of the domain API, result archives
and general artifact storage are not yet implemented.
