# Eyes Studio

The desktop application built on eyes4s: four sbt projects outside the library
aggregate (`docs/studio/DESIGN_SPEC.md` section 13).

| Project | Platforms | Holds |
|---|---|---|
| `studio-core` | JVM, Scala.js | document, commands, services, `StudyBackend` |
| `studio-app` | JVM, Scala.js | pure presentation layer |
| `studio-viz` | JVM, Scala.js | Intaglio scene builders (core, interaction, svg) |
| `studio-desktop` | JVM (JDK 22+) | JavaFX shell: scaladock, Intaglio's JavaFX backend |

> This is pre-release development software. APIs, semantics, and package
> boundaries may change. Nothing here is published.

## Build

```sh
bash studio/publish-pins.sh            # once per pin change; see below
sbt studioAll checkBoundaries studioStyleCheck
```

`studioAll` tests every studio project and links the Scala.js app. The library's
`compileAll` and `testAll` never touch studio.

## Source pins

scaladock and Intaglio are source-only pre-release repositories with no
published artifacts. Each is pinned to one full Git SHA in
[`pins.properties`](pins.properties):

| Provider | Revision | Modules used | Coordinates |
|---|---|---|---|
| Intaglio | `2fa5c682f4a95b8e73daad78dd9302d7aff46e11` | core, interaction, svg (JVM+JS); javafx (JVM) | `io.github.canardlapin::intaglio-*` |
| scaladock | `3ed443e99e53c8af0cf5c5707abcbb70c125f705` | core, fx (JVM) | `io.github.bbuchsbaum::scaladock-*` |

The build resolves each as the ordinary library version `0.0.0-<full SHA>`.
[`publish-pins.sh`](publish-pins.sh) produces those artifacts: it fetches the
pinned commit into `target/studio-pins/`, verifies the checked-out SHA, and runs
the provider's own sbt build with that fixed version into the local Ivy
repository. It never uses a working tree, so uncommitted work cannot reach a
pin. A pin already published is skipped; `--force` rebuilds it.

Why artifacts rather than sbt source dependencies (`ProjectRef` on a Git URI):
a source dependency is loaded with the build, so every library command would
first clone both providers, and a provider that cannot be fetched would break
the library build as well as studio. With artifacts, only studio's dependency
resolution needs the pins, and a missing pin fails there with the artifact name.

To move a pin: edit its revision, run `bash studio/publish-pins.sh`, run the
gate above, and regenerate the workflows (`sbt githubWorkflowGenerate`), which
record the pinned checkout paths.

`--source NAME=<checkout or URL>` fetches the same pinned commit from somewhere
else, for example an offline mirror; the checked-out SHA is still verified.

### "not found: …:0.0.0-<sha>"

If sbt cannot resolve `io.github.canardlapin::intaglio-*` or `io.github.bbuchsbaum::scaladock-*`
at `0.0.0-<sha>`, the pins are not published locally. Run `bash studio/publish-pins.sh`
once per pin change, then retry.

### Re-pinning scaladock

Read the "Behaviour changes — check these when updating a pin" section of scaladock's
CHANGELOG.md for every commit between the old and new pin before changing
`scaladock.revision`. scaladock's CI has a library-floor job (JDK 22, JavaFX 24.0.1),
which is Studio's runtime. Keep the pin on a commit where that job is green.

### What the scaladock pin provides

At `3ed443e` scaladock has asynchronous close admission (`Dock.requestClose`,
`PaneView.prepareClose`, `requestCloseAll`) and true minimize. The tab header
height is `LayoutSettings.headerPx` (default 32), passed to `Dock(...)`. The
header button glyphs are CSS shapes (`.dock-icon.close`, `.minimize`,
`.maximize`, `.popout`, `.dock-back`, `.chevron-down`), which a
`DockTheme.Custom` stylesheet can redefine. scaladock has no host-supplied
header action, so a "more actions" button needs an upstream hook (UP-scaladock).

## Local-checkout override

For co-development, a system property replaces a pin with a local checkout,
built from source as an sbt project reference:

```sh
sbt -Dstudio.intaglio.local=../intaglio -Dstudio.scaladock.local=../scaladock studioAll
```

Each property names a directory containing a `build.sbt`. There is no implicit
sibling lookup: without the property the pin is used, whatever is checked out
nearby. An override compiles the checkout's working tree, including
uncommitted changes, so rerun the gate without it before recording evidence.
scaladock makes warnings fatal when `CI` is set.

## Versions

- Scala: studio compiles with 3.7.4. Intaglio's artifacts are built with 3.3.8
  and scaladock's with 3.7.0; both are readable by 3.7.4.
- JavaFX: 24.0.1 for the shell and both providers. Intaglio (21.0.5) and
  scaladock (24.0.1) declare it `Provided`. scaladock-fx also depends on ScalaFX,
  which only its demo uses and which pulls every OpenJFX module at 24;
  studio-desktop excludes ScalaFX, and `studioDesktop/checkModuleBoundaries`
  (part of `checkBoundaries`) fails on any other OpenJFX version.
- Cats: neither provider depends on Cats; studio uses eyes4s's.

## CI

Every job in `.github/workflows/studio.yml` (generated from `build.sbt`)
publishes the pins from their remotes before sbt runs. The
`studio-clean-clone` job then builds studio through the override properties,
pointed at the pinned checkouts, which proves the override on every push and
pull request that touches studio.
