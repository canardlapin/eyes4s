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
| scaladock | `32ac3ca87d3728d88cdd9d694959dd6ff80b9ddb` | core, fx (JVM) | `io.github.bbuchsbaum::scaladock-*` |

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

### scaladock has no remote yet

The scaladock repository has not been pushed. Its declared home,
`https://github.com/bbuchsbaum/scaladock`, does not resolve, so the default pin
step fails for scaladock on a fresh clone and in CI. Until it is pushed, build
the same committed revision from a local checkout:

```sh
bash studio/publish-pins.sh --source scaladock=/path/to/scaladock
```

The checkout's uncommitted changes are ignored. At the pinned revision
scaladock has true minimize (`32ac3ca`) but not asynchronous close admission
(`Dock.requestClose`, `PaneView.prepareClose`, `requestCloseAll`), which exists
only as uncommitted work. Studio's unsaved-changes close flow needs it, so the
pin must move once that work is committed.

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
scaladock makes warnings fatal when `CI` is set; run an override build with
`env -u CI` if its tree has warnings.

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

The `studio-clean-clone` job in `.github/workflows/studio.yml` (generated from
`build.sbt`) publishes the pins on a fresh runner, runs the studio gate, and
builds once more through the override properties. It runs on demand only
until scaladock can be fetched from its remote.
