# Offline Studio board references

S10.4-render renders the nine editable boards at 1440 × 900 logical pixels,
with 1× and 2× rasters. The application does not depend on this tooling.

```sh
cd tools/studio-boards
npm ci --ignore-scripts
npx playwright-core install chromium
npm run render
npm run verify
```

`package-lock.json` pins Playwright Core 1.49.1 and its Chromium revision 1148
(131.0.6778.33). The runner refuses a different browser version. The committed
manifest also records the OS, architecture and Node version: these are part of
the reference environment, not a promise of identical font rasterisation on
other platforms. Reproduce on that environment before replacing references.

The runner serves only the board documents, the local resolver and the seven
font faces already bundled with Studio. It removes the Google Fonts link in
the served copy, uses explicit local `@font-face` declarations, and refuses
every non-loopback request. The editable boards remain unchanged. It waits
for every face to load, checks the 1440 × 900 root, rejects unresolved template
expressions/directives and incorrectly namespaced SVG shapes, and checks two
successive captures for exact byte identity. `--verify` renders in a fresh
process and compares every PNG and the input/environment manifest. No private
dataset is read.

`docs/studio/design/support.js` is adapted from the Apache-2.0 local resolver in
`bbuchsbaum/storyatlas4s`, `mockups/v2/support.js`, as present at commit
`90f682d1acddd759867098e90f6e3ef163244b0e`; its original file hash is in the
header. Local changes evaluate the embedded `text/x-dc` component, resolve
schema defaults and same-origin board imports, preserve null/boolean attribute
semantics, and support the Figures board's primitive text-fragment operation.
No general React runtime is assumed. Unknown expressions/directives fail.

For table-safe HTML parsing, the server changes `sc-for`/`sc-if` carriers into
inert templates before parsing; JavaScript and comments are preserved. This
prevents HTML foster parenting from silently moving repeated rows out of a
table. The resolver removes those carriers after expansion.

The eighteen PNGs and their manifest live under
`docs/studio/design/reference/`. They are **design reference images**. Signing
off a JavaFX board still requires its story state, checklist/metric assertions,
actual application capture, visual review and recorded deviations. Browser
references are never treated as pixel goldens for JavaFX.
