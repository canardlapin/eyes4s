# A method author outside the eyes4s build

This small project imports published eyes4s APIs and law suites. It adds a comparison with a
validated multiplier, a distinct score type and an integer-valued custom trial key. It also
registers an extension-owned detector parameter type and codec around the canonical I-VT
implementation, preserving the algorithm card carried by the resulting detection artifact. Its
tests save/reload both study and recording plans, compare independently generated numerical
targets, export results, and exercise missing/duplicate registration and malformed schemas.
They also save the custom plan, input and result archive under a manifest and resolve it from an
in-memory source through the consumer's own registrations, under the published `ManifestLaws`.

From the eyes4s repository root:

```sh
python3 tools/study-consumer/verify.py
```

The script publishes JVM and Scala.js artifacts to the local Ivy cache under
`0.0.0-workflow-slices`, copies only this consumer's build and sources to a fresh temporary directory,
and runs both suites there. It verifies that the consumer classpath uses the packaged artifacts and
contains no path to the library checkout. The printed directory retains logs and a receipt. No
artifact is published remotely. `--skip-publish` is for rerunning against the already-built local
version; after changing library sources, run the full command again.

The custom multiplier predicts a proportional change in every contrast at each scale. Targets
come from the independent closed-form calculation in `tools/r-parity/generate_multiscale.py`, not
from an eyes4s run. This example demonstrates the comparison extension boundary; custom detector
registration is also exercised through packaged artifacts on both runtimes. A new detector
algorithm would additionally supply its own machine and truthful algorithm card. Smoother
registration, progress and cancellation remain later application-foundation work.
