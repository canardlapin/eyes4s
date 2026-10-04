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
| `files` | `FileSystem` | `read`, `readStream` (chunks, for large ASC sources), `write`, `list` and `child` for host paths; `project` opens a bundle as a `ProjectStore` | `JvmFileSystem` | `InMemoryPlatform` |
| `dialogs` | `Dialogs` | open, save and directory choosers (`chooseDirectory` picks a project bundle, which is a directory); `None` means the user cancelled | `JavaFxDialogs` (`FileChooser`, `DirectoryChooser`) | scripted answers |
| `clipboard` | `Clipboard` | plain text in and out. A host that may not read returns `Unsupported` and clears `clipboardRead` | `JavaFxClipboard` | in memory |
| `fonts` | `Fonts` | registration of the bundled faces before the first frame; any other resource is refused | `JavaFxFonts` | recorded |
| `scheduler` | `Scheduler` | wall clock with the UTC offset (`ClockReading`) and one-shot delayed tasks | `Scheduler.temporal` with `JvmZone` | virtual clock |
| `external` | `ExternalOpen` | `https:`/`http:`/`mailto:` URLs in the default application; reveal a path in the file browser | `DesktopExternalOpen` (HostServices) | recorded |
| `preferences` | `Preferences` | per-user key/value strings that outlive a project | `JvmPreferences` (`java.util.prefs`) | in memory |

`HostPath` is opaque. On the desktop it is a file-system path; in a browser it can be the id of a
File System Access handle. Only the host's `FileSystem` interprets it. `HostPath.of` refuses blank
text and control characters (NUL included), and `child` refuses a name that is blank, `.`, `..`, or
holds a separator or a control character. A path the host cannot represent (a Windows-reserved
character, say) is `InvalidPath`. No `FileSystem` operation throws: every failure is a
`PlatformError` naming the path.

`PlatformConformance` fixes the refusals every host gives:

- A directory, the root included, is `Unwritable` to `write` and `Unreadable` to `read` and
  `readStream`.
- A file is `Unreadable` to `list`, and a path that does not exist is `Missing`.

`readStream(path, chunkSize)` checks the path when it opens and refuses it as a value. A failure
after that raises a `PlatformFailure` in the stream. The JVM reads the file incrementally, and the
in-memory host chunks its bytes.

A host without a local file system sets `HostCapabilities.localFiles` to false. Its
`files.project` answers `Unsupported("files", "projects")`; it does not fake a bundle.

`FontRequest.resource` is a JVM classpath resource: studio-desktop bundles the faces under
`eyes4s/studio/desktop/fonts/`. A web host maps each resource to the URL it serves that file at
(an `@font-face` source) and registers the same family names.

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
  client. On stdio, **stdout carries protocol lines only**. The sidecar writes every log line,
  warning and stack trace to stderr, so a stray `println` never corrupts the stream.

The wire format is `WireFormat`:

1. Each message is one JSON envelope, `{"version":{"major":1,"minor":2},"id":<long>,"body":…}`,
   encoded as UTF-8 and terminated by `\n` (NDJSON). JSON escapes newlines inside strings, so one
   line always holds one envelope. Over a WebSocket, each text message is one line without its
   terminator. A line may be at most `WireFormat.MaxLineLength` characters (16 MiB). Neither side
   buffers past that limit.
2. The client sends `Envelope[BackendRequest]` with an id that is unique on the connection.
3. The server answers every request with frames (`Envelope[ServerFrame]`) that carry the request's
   id:
   - one `Response` frame for every request except `Subscribe`;
   - for `Subscribe`, `Event` frames ending in exactly one `Finished`, or a single
     `Response(Refused)` if the job is unknown. A subscription that is ended early by `Unsubscribe`
     ends without `Finished`.
4. **Unsubscribe and close.** `Unsubscribe(subscription)`, protocol 1.1, ends the subscription
   that request `subscription` opened on this connection. It is answered by
   `Unsubscribed(subscription, active)`. That response is sent after the subscription's last frame,
   so no frame of the subscription follows it. `active` is false when the subscription had already
   ended or never existed. Unsubscribing does not cancel the job; `Cancel` does. Closing the
   connection ends all of its subscriptions. In process, a subscription is ended by dropping its
   stream, and `Unsubscribe` answers `Unsubscribed(_, false)`.
5. The server serves requests concurrently. Frames of different requests interleave on the
   connection, so the client demultiplexes them by id. A live subscription never holds up a later
   request.
6. Versions: the server refuses a request of another major version with
   `Refused(UnsupportedVersion)`. The client treats a frame of another major version as a transport
   defect (`TransportError.Incompatible`). Protocol 1.1 added the S0.9 request and refusal
   variants; 1.2 added `ProgressTotal.Counting`, and 1.3 encodes Long values outside the safe
   JSON integer range as canonical decimal strings. `WireFormat` parses numbers exactly on both
   platforms; a port must also prevent fractional numeric text from rounding into an integer
   before validating a count. Client and backend must be upgraded together:
   mixed-minor deployments are unsupported. The transport decodes a typed body before checking
   the major version and does not negotiate minor capabilities. An older decoder cannot read a
   new variant; changing the envelope's version label does not change that. See the
   [protocol regression](README.md#backend-protocol-versions) for the legacy-total decoder probe.
7. Refusals are values (`BackendError`). A frame for the wrong id, or a missing, duplicated or
   mismatched response, is a `TransportError`, raised as a `TransportFailure`.
8. **Malformed lines.** When a request line does not decode but its `id` can be read, the server
   answers under that id with `Refused(Malformed(excerpt, reason))` and the connection goes on. A
   line whose id cannot be read cannot be answered: the server ends the connection with
   `TransportError.Unidentifiable`, whose message says so. A line longer than the limit ends the
   connection with `TransportError.LineTooLong`. An error quotes at most
   `WireFormat.ExcerptLength` characters (200) of a bad line, never the whole line.

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
  through `StudioFonts.loadAll`, and `AppEffect.RevealProject` is not performed. The services
  exist and pass conformance. The wiring is tracked on S2.9.
- `RemoteStudyBackend` opens one exchange per request. A shell that holds one long-lived
  connection demultiplexes frames by id itself (rule 5 above). No such client ships yet.
- There is no Scala.js shell yet. Porting a shell's own code is not part of S0.9.
