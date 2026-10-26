# JavaFX regression matrix

`VisualParitySuite` boots all five perspectives at t1/t2/t3 in light and dark:
30 executed native cases, each captured at snapshot scales1 and2. The public
fixture input port is shared with `BoardParityCaptureFxSuite`; these are real
application windows, not separately drawn specimens. Sources are digest-checked.
Stored maps/assets, terminal report replies and relevant preview/figure inputs
must arrive before capture. The scene root has programmatic focus, with no
keyboard ring or text caret, and two successive captures must agree.
Snapshots explicitly cover the scene rectangle at each raster scale. A child's
stroke or effect outside that rectangle must not move the image origin; the
harness tests this with deliberately overflowing children at both scales.

Pixel comparison runs only on Linux Monocle at native output scale1, software
pipeline and bundled Plex fonts. macOS and native GTK2 execute the same cases as
functional/repeat checks; actual output-scale2 sharpness is its independent court.
Browser design boards remain semantic reviews, not these pixel baselines.

The named policy allows at most one channel level of raster rounding in at most
0.1% of pixels, and refuses wrong dimensions. Earlier complete-suite repeat
captures measured344/115 changed pixels at1440×900, each changing by one level;
this policy still has to pass repeated captures in the Linux baseline environment.
`VisualPixelsSuite` verifies refusal with deliberate visible pixel mutations,
broad one-level changes, missing files and wrong image dimensions. There is no
automatic baseline-update mode. A missing baseline fails after saving the actual
capture and receipt; reviewed Linux capture artifacts are added in a separate
commit. Retain that initial red bootstrap as unqualified evidence.

```sh
sbt 'studioDesktop/testOnly *VisualParitySuite *VisualPixelsSuite'
python3 -m unittest discover -s tools/studio-visual-parity -p 'test_*.py'
python3 tools/studio-visual-parity/check.py
```

Native macOS may clamp the scene to its runner screen. Functional receipts record
the actual logical size and require both PNG sizes to match it; the Linux golden
court still requires exactly1440×900 logical pixels. Rendering readiness and
repeat checks remain mandatory in both modes.

On macOS, explicitly use `check.py --allow-functional-only`; its receipt says that
Linux pixel qualification was not performed. CI's headless Linux job requires
all30 cases executed (no failures, skips or duplicates), all60 captures, source and
reviewed baseline hashes, exact dimensions and both repeat/baseline comparisons.
The source fingerprint includes library/Studio sources and tests, resources/fonts,
public fixture inputs, project/root build settings and the court tools; pixel
baselines are hashed separately. Each state records its JDK, JavaFX, OS, Glass
and software-pipeline properties. Editing another production file cannot reuse
old captures even if the capture suite is unchanged.
Diff PNGs are retained on pixel failure. Changing thresholds requires new evidence
and review; CI never accepts a changed image merely because it can capture it.

The macOS and GTK2 jobs also require every matrix XML case to execute and pass,
using `--allow-functional-only`; they preserve their actual captures, receipts and
the complete responsive Results Tab transcript as separate artifacts. These
functional courts cannot substitute for the Linux pixel-golden court.
