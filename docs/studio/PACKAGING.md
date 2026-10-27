# Eyes Studio macOS candidate

S10.7 builds an internal test application. This is not a public 1.0 release and
is not signed with Developer ID or notarized. macOS requires a positive first version component, so package metadata uses
1.0.0 for this internal candidate; this is not a published release version. `package.json` and the application's About/error report identify the exact
source commit. The owner selected unsigned internal testing on 2026-10-27; public signing and
notarization are deferred.

## Build

Use a clean, agent-owned standalone checkout, the pinned Studio dependencies, and
JDK 25 from `~/Library/Java/JavaVirtualMachines` (see `studio/README.md`).

```sh
export JAVA_HOME="$(/usr/libexec/java_home -v 25)"
export PATH="$JAVA_HOME/bin:$PATH"
python3 tools/studio-package/build.py --output /private/tmp/eyes-studio-candidate
```

The output directory must not exist. The script stages `Runtime/fullClasspath`,
packages class directories as JARs and preserves dependency JARs. It rejects test
framework/Monocle leakage, includes the JDK runtime, bundled example, fonts and
third-party notices, and emits an architecture-specific ZIP. It checks the source
fingerprint before and after staging. `--allow-dirty` is for iteration only and
records that state; a dirty artifact cannot qualify a release candidate.

`jpackage` builds only for the host architecture. Build Intel and Apple Silicon
artifacts on their respective hosts; a successful arm64 build is not Intel proof.
The final bundle has an **ad-hoc** integrity seal, not a trusted distribution
signature. Do not instruct testers to disable Gatekeeper or strip quarantine.
A Developer ID/notarization route, if selected, must seal the final document-type
metadata, submit the archive, staple and assess it before distribution acceptance.

The runtime explicitly includes Java SE, native-access support, crypto, management,
JFR, locales, charsets and ZIP filesystems. It does not bundle compiler/linker tools.

The `.eyes` document type is an exported directory-package UTI. CLI paths and
macOS Open File events enter the ordinary project lifecycle, including its dirty
save prompts, locks and operation-in-progress refusals. Multiple simultaneous
opens remain subject to the existing single-project operation policy.

The app icon is a vector eye using `Tokens`' light Accent/OnAccent/AccentSoft.
Regenerate on macOS with `python3 tools/studio-package/icon.py`; the headless
AppKit source is retained beside the generated `.icns`.

## Automated check and its limits

The builder runs the **packaged launcher**, with Java environment variables
removed, a minimal system PATH and an unrelated temporary working directory:

```sh
'/path/to/Eyes Studio.app/Contents/MacOS/Eyes Studio' \
  --package-check /private/tmp/new-package-check
```

This checks fonts/notices, reads the bundled example, copies it through the
production store, reopens it and verifies every stored input. The receipt must
identify a `java.home` inside the app and the expected source SHA. Existing check
output is refused. No graphics toolkit or OS window is started by this check.

The generated Studio workflow builds and uploads the macOS candidate and raw
receipts. This is build/runtime evidence, **not** a claim of Finder dispatch,
VoiceOver, physical Retina or clean macOS 14 acceptance.

## PackagedSmokeTest — clean macOS 14

Use a disposable macOS 14 VM, no installed JDK, and the candidate's intended
transfer route (retain quarantine state). Record candidate ZIP SHA-256, source
SHA, architecture, OS build, runtime receipt and signing/notarization decision.

1. Verify the ZIP hash, unpack, move the app to Applications and launch it from
   Finder. Record Gatekeeper's actual response; stop and report a refusal.
2. Open the bundled example through File → Open Example. Choose a writable copy
   destination. Visit Data, Explore, Analysis, Compare and Figures; retain
   screenshots and confirm inputs, recipe and analysis identifiers are visible.
3. Save the project, quit, and double-click the saved `.eyes` package in Finder.
   Confirm it opens in Studio, with the same revisions, runs and stored inputs.
4. With Studio already running, double-click another project. Modify the current
   project first and verify the save/discard/cancel prompt; cancel keeps it open.
5. Repeat opening with a project whose path contains spaces and non-ASCII text.
   A locked, invalid or missing project must show its normal explicit refusal.
6. Run `--package-check` from Terminal with JAVA_HOME unset; retain the receipt.
   This complements the GUI journey; it cannot substitute for it.
7. Review the physical Retina display and perform the VoiceOver script in
   `A11Y_AUDIT.md`. Record actual backing scale, input method and observed output.

Keep step-level pass/fail/not-run results and screenshots. S10.7 stays open until
the VM journey is recorded; the internal unsigned distribution decision is recorded. A hosted macOS 15 run
cannot silently replace the specified macOS 14 VM.

## macOS document-event bridge

JavaFX 24's [Glass Application](https://github.com/openjdk/jfx/blob/jfx24/modules/javafx.graphics/src/main/java/com/sun/glass/ui/Application.java)
receives document opens and replays startup documents when its handler changes.
The public JavaFX Application API has no document-open callback. `MacProjectFiles`
is therefore a deliberately isolated internal-API adapter for the pinned JavaFX
version, with **every** other event delegated to the prior toolkit handler,
including quit and platform appearance changes. A conformance test refuses a
future upstream event method that the adapter fails to forward.

Initializing AWT before JavaFX would change native toolkit/menu ownership; do not
use that as an event workaround. This adapter needs native Finder dispatch proof
on the packaged candidate, in addition to its headless delegation tests. No
module export switches are required because Studio currently runs on the classpath.
A future modular launcher must explicitly revisit this internal API dependency.

Document metadata follows Apple's [document type declaration guidance](https://developer.apple.com/documentation/uniformtypeidentifiers/defining-file-and-data-types-for-your-app).
Packaging uses the JDK's [jpackage tool](https://docs.oracle.com/en/java/javase/25/docs/specs/man/jpackage.html).

## Hosted native package smoke

On its ephemeral test desktop, the macOS CI job also runs `native-smoke.py`. It
injects a **test-only observer JAR** (never part of the app ZIP), launches the
packaged app using LaunchServices, then sends a separate OS Open File event for a
copied project with spaces and non-ASCII text in its name. The observer cannot
open or seed a project. It requires the expected window title, native Mac Glass,
native-menu policy and a nonblank scene, captures that scene, then requests the
ordinary window-close lifecycle. The runner requires normal exit and the bundled
JVM path. Its receipt and PNG are uploaded with the candidate.

This supplements the non-graphical package check and catches missing native
libraries, broken document dispatch and startup rendering. It does not replace
Finder UX review, actual spoken accessibility, physical Retina or the specified
clean macOS 14 VM. The script refuses to run without `--allow-visible`; never run
it on a desktop someone is using.
