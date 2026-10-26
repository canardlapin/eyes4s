# Saved-part retention

A successful save may reclaim document parts referenced by the retired previous manifest.
The current manifest and the newly retained previous manifest keep all their parts. Cleanup
runs only after the manifest swap and successful journal reset, while holding the session
mutex and writer lock. It verifies the current and previous manifest bytes, the clean active
journal and all three document generations before deleting any retired part.

Any entry in `journals/` defers reclamation. Archived journals are historical recovery data;
their start digest alone does not provide a complete manifest index. Pinning all generations
is deliberate until an explicit archive/base index is implemented. Unknown or corrupt
references, failed reads and failed journal resets also defer cleanup.

Only parts explicitly named by the retired manifest can be removed. Inputs, run artifact
archives, journal archives, caches, unrelated files and older unindexed orphan parts are
untouched. This bounds growth from ordinary repeated saves without guessing the ownership
of pre-existing files. Deferred or failed cleanup can leave extra files. There is no promise
that every unreachable file will be collected.

`SaveReceipt.retention` reports collected paths, archival deferral or storage failures.
A deletion failure does not invalidate a completed save. The desktop forwards failures
through its existing save-warning presentation. A crash during cleanup still leaves both
current and previous saves readable. Evidence is tracked by
`bd-01M44PBGT3KAFQ160QTEV3G3HK`; closure requires memory and real-filesystem fault tests,
mutation checks and the complete landing gate.
