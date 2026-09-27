# Porting Eyes Studio to another shell

Eyes Studio ships as a JavaFX desktop application, but its behaviour does not live in JavaFX
(DESIGN_SPEC §13). This document is the port contract (ticket S0.9). It names every seam a new shell
crosses, what the shell has to implement at each one, and the acceptance a port has to pass before
it counts as Eyes Studio. The likely hosts are Electron or Tauri with Scala.js, or a plain browser.

A shell renders view-models and dispatches intents. It holds no behaviour: if a port needs
behaviour that the shared modules do not have, add it to `studio-app` or `studio-core` with a
headless test, not to the shell.

## Layers

| Project | Platforms | May the shell replace it? |
|---|---|---|
| `studio-core`: document, commands, services, `StudyBackend`, platform interfaces | JVM, Scala.js | No. The shell reuses it. |
| `studio-app`: app model, intents, pure `update`, view-models, `LayoutSpec`, strings, tokens | JVM, Scala.js | No. The shell reuses it. |
| `studio-viz`: Intaglio scene builders | JVM, Scala.js | No. The shell renders its scenes with an Intaglio backend (SVG or canvas on the web). |
| `studio-desktop`: JavaFX shell and JVM platform services | JVM | Yes. This is what a port rewrites. |

`sbt checkBoundaries` enforces the split. No `javafx`, `scaladock.fx`, `java.io`, `java.nio.file` or
`java.nio.channels` name may appear in the portable projects, and no `cats.effect` or `fs2` name may
appear in `studio-app` or `studio-viz`. CI links `studioAppJS` on every run.

## The seams

Each row says who owns the seam's code and whether S0.9 introduced it.

| # | Seam | Defined in | A new shell must | Desktop implementation |
|---|---|---|---|---|
| 1 | Elm loop | `studio-app`: `AppModel.update`, `Intent`, `AppEffect` (existing) | hold one `AppModel`, apply each intent with `AppModel.update`, re-render, and perform the returned effects in order | `desktop.runtime.StudioRuntime` |
| 2 | View-models | `studio-app`: `vm.Shell.project`/`context`/`banner`/`status`, `vm.Menus`, `vm.A11y`, `nav`, `jobs` (existing) | render them as given. Strings, enablement, tones and accessible names are already computed | `desktop.shell.*` |
| 3 | Commands and keys | `studio-app`: `keys.CommandRegistry`, `keys.Keymap` (existing) | bind the registry's shortcuts, using the host's menu model when `HostCapabilities.nativeMenuBar` is set | `desktop.shell.ShellKeys`, `AppBar` |
| 4 | Layout | `studio-app`: `layout.LayoutSpec`, `StudioLayouts` (existing) | map every `PerspectiveLayout` to the host's docking library and report layout changes back as intents | `desktop.dock.DockLayouts` (scaladock) |
| 5 | Tokens and type | `studio-app`: `tokens.Tokens`, `TokenCss.web`, `TypeCss` (existing) | load the generated web CSS variables (`TokenCss.web`), never literal colours | `desktop.tokens.TokenFiles` (JavaFX CSS) |
| 6 | Strings | `studio-app`: `text.Messages`, `Catalogue` (existing) | take every visible string from `Messages` | the same |
| 7 | Scenes | `studio-viz`: `StudioScenes`, `plot`, `trial` (existing) | draw the scenes with an Intaglio backend and feed pointer and keyboard input back through `trial.TrialInput` | `desktop.plot.CanvasPlotHost`, `desktop.trial.*` |
| 8 | Effects | `studio-app`: `AppEffect` (existing) | perform each case: execution to the backend, dialogs, journal and save to the project session, layout reset, dock commands | `desktop.runtime.DesktopEffects` |
| 9 | Platform services | `studio-core`: `platform.Platform` (new in S0.9) | implement all eight members (below) | `desktop.platform.DesktopPlatform` (new) |
| 10 | Project storage | `studio-core`: `bundle.ProjectStore` (existing, S2.3); opened through `FileSystem.project` (new) | provide a store per bundle that passes `ProjectStoreConformance` | `desktop.platform.FileProjectStore` |
| 11 | Backend transport | `studio-core`: `backend.BackendTransport`, `RemoteStudyBackend` (new) | either run the backend in process (`BackendTransport.inProcess`) or speak the sidecar wire protocol (below) | in process, `FakeStudyBackend` until S3.7 |
| 12 | Headless services | `studio-core`: `headless.StudioServices` (existing, S3.6) | nothing. It is the test-side counterpart of seams 8 and 11 | `headless.HeadlessSession` |

### Platform services (seam 9)

`eyes4s.studio.core.platform.Platform[F]` bundles the host's services. The shell constructs one at
startup. Every recoverable failure is a `PlatformError` value that names its operands. An effect
fails only on a defect of the host.

| Member | Interface | What the host provides | Desktop | Portable reference |
|---|---|---|---|---|
| `capabilities` | `HostCapabilities` | native menu bar, several windows, local files, clipboard reads | `DesktopPlatform.capabilities` | all on |
| `files` | `FileSystem` | read, write, list and `child` for host paths; `project` opens a bundle as a `ProjectStore` | `JvmFileSystem` | `InMemoryPlatform` |
| `dialogs` | `Dialogs` | open and save choosers; `None` means the user cancelled | `JavaFxDialogs` | scripted answers |
| `clipboard` | `Clipboard` | plain text in and out. A host that may not read returns `Unsupported` and clears `clipboardRead` | `JavaFxClipboard` | in memory |
| `fonts` | `Fonts` | registration of the bundled faces before the first frame; any other resource is refused | `JavaFxFonts` | recorded |
| `scheduler` | `Scheduler` | wall clock with the UTC offset (`ClockReading`) and one-shot delayed tasks | `Scheduler.temporal` with `JvmZone` | virtual clock |
| `external` | `ExternalOpen` | `https:`/`http:`/`mailto:` URLs in the default application; reveal a path in the file browser | `DesktopExternalOpen` (HostServices) | recorded |
| `preferences` | `Preferences` | per-user key/value strings that outlive a project | `JvmPreferences` (`java.util.prefs`) | in memory |

`HostPath` is opaque. On the desktop it is a file-system path; in a browser it can be the id of a
File System Access handle. Only the host's `FileSystem` interprets it.

A browser host gets its UTC offset from `-new Date(ms).getTimezoneOffset()` and passes it to
`Scheduler.temporal`.

### Backend transport and the sidecar protocol (seam 11)

`StudyBackend[F]` is the only way studio reaches eyes4s. Its requests (`BackendRequest`), responses
(`BackendResponse`) and progress events (`JobEvent`) have cross-built circe codecs. A client reaches a
backend through a `BackendTransport`:

- **In process.** `BackendTransport.inProcess(backend)` calls `StudyBackend.handle` directly. This is
  the JVM desktop today, and Scala.js later, because the eyes4s plan, codec and fs2 modules
  cross-build.
- **IPC sidecar.** A JVM process runs `SidecarServer.serve(backend)` over its stdin and stdout or a
  WebSocket, and the shell speaks the wire format below. A Scala.js shell can use
  `RemoteStudyBackend` over its own transport. A shell in another language implements the same
  client.

The wire format is `WireFormat`:

1. Each message is one JSON envelope, `{"version":{"major":1,"minor":0},"id":<long>,"body":…}`,
   encoded as UTF-8 and terminated by `\n` (NDJSON). JSON escapes newlines inside strings, so one
   line always holds one envelope. Over a WebSocket, each text message is one line without its
   terminator.
2. The client sends `Envelope[BackendRequest]` with an id that is unique on the connection.
3. The server answers every request with frames (`Envelope[ServerFrame]`) that carry the request's
   id:
   - one `Response` frame for every request except `Subscribe`;
   - for `Subscribe`, `Event` frames ending in exactly one `Finished`, or a single
     `Response(Refused)` if the job is unknown.
4. The server serves requests concurrently. Frames of different requests interleave on the
   connection, so the client demultiplexes them by id. A live subscription never holds up a later
   request.
5. Versions: the server refuses a request of another major version with
   `Refused(UnsupportedVersion)`. The client treats a frame of another major version as a transport
   defect (`TransportError.Incompatible`). Minor versions only add.
6. Refusals are values (`BackendError`). A malformed line, a frame for the wrong id, or a missing or
   mismatched response is a `TransportError`, raised as a `TransportFailure`. The server ends a
   connection that sends a malformed line.

`RemoteStudyBackend.subscribe` first asks for the job with `Job`, so that an unknown job is refused
before a stream is returned. The subscription itself starts when the stream runs.

## What a port must pass

A port is accepted when all of the following hold. None of them may be weakened for a port.

1. **Shared modules unchanged in behaviour.** The port adds no behaviour to its shell, and
   `sbt studioAll checkBoundaries studioStyleCheck` is green, including the JS test runs and
   `studioAppJS/fastLinkJS`.
2. **UI-neutral acceptance suite.** These studio-app and studio-core suites pass on the platform the
   port runs its shared code on (JVM, or JS for a Scala.js shell):
   - `AppUpdateLawsSuite`, `GoldenJourneyHeadlessSuite`, `StudioDriverSuite`, `ViewModelSnapshotSuite`,
     `MenusSuite`, `CommandRegistrySuite`, `LayoutSpecSuite`, `ExplainNavigationSuite`,
     `TrailModelSuite`, `MessagesSuite`, `TokenContrastSuite` and `TokenCssSuite` (studio-app);
   - `BackendConformanceSuite` over every backend the port uses;
   - `ProjectStoreConformance` over every project store the port provides.
3. **Platform services.** The port's `Platform` passes `PlatformConformance`, as
   `PlatformInterfacesSuite` (in memory) and `DesktopPlatformSuite` (JVM) do. Dialogs are
   interactive: the port adds a test of its chooser configuration, as `DesktopPlatformSuite` does
   for JavaFX.
4. **Transport.** Every transport the port adds extends `TransportConformanceSuite`. That runs the
   whole `BackendConformanceSuite` through `RemoteStudyBackend` and requires the transport's frames
   to equal the in-process frames, as `InProcessTransportSuite` and `LoopbackTransportSuite` do. A
   port in another language replays the same requests against `SidecarServer` and compares frames.
5. **Visual parity.** Every board in `PARITY_CHECKLIST.md` is signed off for the port, in both
   themes, at 1440×900, as the S10.4 tickets do for the desktop.
6. **Accessibility.** The port exposes the view-models' accessible names and roles (`vm.A11y`) to
   the host's accessibility tree, and follows the keyboard rules of DESIGN_SPEC §10 and §12.

## Known gaps

- The desktop does not yet construct `DesktopPlatform` in `StudioApplication`. Fonts still load
  through `StudioFonts.loadAll`, and `AppEffect.RevealProject` is not performed until S2.9. The
  services exist and pass conformance; S2.9 wires them in.
- `RemoteStudyBackend` opens one exchange per request. A shell that holds one long-lived
  connection demultiplexes frames by id itself (rule 4 above). No such client ships yet.
- There is no Scala.js shell yet. Porting a shell's own code is not part of S0.9.
