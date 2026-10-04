# Rendered documentation review

Base commit: `413a351d` (origin/main, "Record the P0 batch 3 landing"). The site was rebuilt
locally with `sbt docs/tlSite` from that tree, together with this branch's documentation fixes. This
is local candidate evidence, not a hosted CI run. No site was deployed.

## Method

[`review.mjs`](documentation-2026-10-04/review.mjs) serves `site/target/docs/site` on a loopback
port. It opens each of the 11 public pages in headless Chromium at 1440×1000 and 390×844. The
browser is Playwright 1.62.1's own build (Chromium 151.0.7922.34, resolved from the user's Node
modules). The project has no Playwright dependency. No browser was installed and the system Chrome
was not used. For every page and viewport the script records:

- title, first heading and sidebar navigation;
- console errors, page errors, failed local requests and HTTP errors;
- horizontal overflow, which Helium hides from the document because `#container`, not the page,
  scrolls; the script compares the container's scroll width with its width and lists the
  innermost elements that extend past it without a scrolling box of their own;
- code blocks wider than their panel;
- at 390 px, whether the sidebar is off screen before `#nav-icon` is tapped and on screen after,
  and whether a sidebar link navigates.

The script also checks every local link and anchor across both viewports and every repository link
against the files in this checkout. It takes a full-length screenshot after the measurements.
Ownership followed the repository browser policy:

- `node ~/.local/share/agent-policy/browser-automation-guard.mjs --audit` reported "No automated
  top-level browser processes found" before and after every run;
- the receipt records the script's PID and the browser processes it started; none was alive after
  the run;
- the HTTP server, contexts and browser were closed in a `finally` block.

## Finding and fix

On a phone the site scrolled sideways on six pages, so text ran off screen. Helium sets
`white-space: nowrap` on all inline code, so a long inline signature, digest or shell command
widened the page. The migration table was also wider than the screen.

| Page | Widest element at 390 px |
|---|---|
| fixation-studies | `Smoother.anisotropic(sigmaX, sigmaY, edges).density(measure, grid)` (555 px) |
| getting-started | the `publishLocal` and dependency commands (499 px) |
| migration | the three-column route table (700 px) |
| recordings | a 64-character SHA-256 (539 px) |
| reference | the per-module `doc` command (653 px) |
| templates | `PartialAssociation.of(..., AssociationMethod.Spearman)` (560 px) |

The fix is the stylesheet [`site-docs/css/narrow-screens.css`](../../site-docs/css/narrow-screens.css),
linked through Helium (`tlSiteHelium ~= (_.site.internalCSS(Root / "css"))` in `build.sbt`).

- Inline code in prose may wrap, and breaks inside a token only when the token cannot fit a line.
- Code blocks keep Helium's `pre-wrap`.
- Tables keep identifiers whole and scroll inside their own box instead of widening the page.

No page text changed. The 1440 px layout is unchanged.

[`control-receipt-without-stylesheet.json`](documentation-2026-10-04/control-receipt-without-stylesheet.json)
is the same review of the same build with only the stylesheet link removed. It reports exactly
those six pages, which shows the overflow check detects the defect.

## Result

[`receipt.json`](documentation-2026-10-04/receipt.json) covers all 22 page and viewport
combinations, with 0 errors:

- no console or page errors, failed local requests or HTTP errors;
- no horizontal overflow and no code block wider than its panel;
- at 390 px on every page, the sidebar is off screen until the menu is tapped, on screen after, and
  its link navigates;
- 142 local links and anchors and 9 repository links all resolve; the one external link
  (`https://typelevel.org/Laika/`) was not fetched.

Screenshots:

- [templates, 390 px](documentation-2026-10-04/390-templates.html.png), including the new
  decomposition section;
- [migration table, 390 px](documentation-2026-10-04/390-migration.html.png);
- [recordings digest, 390 px](documentation-2026-10-04/390-recordings.html.png);
- [templates, 1440 px](documentation-2026-10-04/1440-templates.html.png);
- [summaries, 1440 px](documentation-2026-10-04/1440-summaries.html.png).

The other 17 screenshots were inspected locally and are not committed.

The 2026-09-19 review reported no overflow. Its probe is not preserved, so whether these pages
regressed since then or were missed by it is not established. An earlier draft of this review's
own probe treated any element inside a scrolling ancestor as contained; Helium's `#container` is
such an ancestor, so that draft could not detect the defect, and it was corrected before these
receipts were recorded.

Repository links name source paths in this checkout. This local review does not claim the
corresponding files are already published at their hosted destinations.
