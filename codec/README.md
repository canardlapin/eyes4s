# eyes4s-codec

Versioned JSON codecs and typed persistence registration for fixation-study plans.
`VersionedCodec[A]` returns typed failures on encode/decode. `StudyCodec` captures the method,
key layout, key codec and parameter codec before registration. Generic maps encode as entry arrays
and reject duplicate keys. Inputs are separate typed content references, not embedded numeric blobs.

Start with [saved studies](../docs/SAVED_STUDIES.md) or the
[extension guide](../docs/EXTENDING_STUDIES.md). Codecs for the rest of the domain API, result archives
and general artifact storage are not yet implemented.
