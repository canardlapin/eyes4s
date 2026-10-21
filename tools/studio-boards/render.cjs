/* Copyright 2026 canardlapin. Licensed under Apache-2.0. */
"use strict";

const fs = require("node:fs");
const path = require("node:path");
const http = require("node:http");
const crypto = require("node:crypto");
const os = require("node:os");
const { chromium } = require("playwright-core");
const root = path.resolve(__dirname, "../..");
const boards = path.join(root, "docs/studio/design");
const output = path.join(boards, "reference");
const fonts = path.join(root, "studio/desktop/src/main/resources/eyes4s/studio/desktop/fonts");
const names = ["System", "DataEmpty", "Data", "Explore", "Analysis", "Main", "MainDark", "Results", "Figures"];
const browserVersion = "131.0.6778.33";
const sha = bytes => crypto.createHash("sha256").update(bytes).digest("hex");
const faces = [
  ["IBM Plex Sans", 400, "IBMPlexSans-Regular.ttf"],
  ["IBM Plex Sans", 500, "IBMPlexSans-Medium.ttf"],
  ["IBM Plex Sans", 600, "IBMPlexSans-SemiBold.ttf"],
  ["IBM Plex Mono", 400, "IBMPlexMono-Regular.ttf"],
  ["IBM Plex Mono", 500, "IBMPlexMono-Medium.ttf"],
  ["Source Serif 4", 400, "SourceSerif4-Regular.ttf"],
  ["Source Serif 4", 600, "SourceSerif4-Semibold.ttf"]
];
const localFonts = faces.map(([family, weight, file]) =>
  `@font-face{font-family:"${family}";font-weight:${weight};font-style:normal;font-display:block;src:url("/fonts/${file}") format("truetype")}`).join("\n");

// HTML parsers otherwise foster-parent custom sc-for tags out of tables. Template
// is the browser's inert, table-safe carrier; SVG children retain their namespace.
function adaptedSource(source) {
  return source.replace(/(<script\b[\s\S]*?<\/script>)|<!--[\s\S]*?-->|<sc-(for|if)\b|<\/sc-(?:for|if)>/gi,
    (match, script, kind) => script || match.startsWith("<!--") ? match : kind
      ? `<template data-dc-directive="${kind.toLowerCase()}"` : "</template>")
    .replace(/<link\b[^>]*href=["']https:\/\/fonts\.googleapis\.com[^>]*>/gi, "")
    .replace("</head>", `<style data-local-fonts>${localFonts}</style></head>`);
}

function serve(req, res) {
  try {
    const name = new URL(req.url, "http://127.0.0.1").pathname;
    let file;
    if (/^\/[A-Za-z]+\.dc\.html$/.test(name) && names.includes(name.slice(1, -8))) file = path.join(boards, name);
    else if (name === "/support.js") file = path.join(boards, "support.js");
    else if (name.startsWith("/fonts/") && faces.some(f => name === `/fonts/${f[2]}`)) file = path.join(fonts, name.slice(7));
    else { res.writeHead(404); res.end(); return; }
    const html = file.endsWith(".html");
    res.setHeader("Content-Type", html ? "text/html; charset=utf-8" : file.endsWith(".js") ? "application/javascript" : "font/ttf");
    res.end(html ? adaptedSource(fs.readFileSync(file, "utf8")) : fs.readFileSync(file));
  } catch (error) { res.writeHead(500); res.end(String(error)); }
}

async function main() {
  const verify = process.argv.includes("--verify");
  const server = http.createServer(serve);
  await new Promise(resolve => server.listen(0, "127.0.0.1", resolve));
  const base = `http://127.0.0.1:${server.address().port}`;
  let browser;
  try {
    browser = await chromium.launch({ headless: true, args: ["--font-render-hinting=none", "--disable-lcd-text"] });
    if (browser.version() !== browserVersion) throw new Error(`Expected Chromium ${browserVersion}, got ${browser.version()}`);
    const assets = Object.fromEntries([
      ...names.map(n => path.join(boards, `${n}.dc.html`)), path.join(boards, "support.js"),
      __filename, path.join(__dirname, "package-lock.json"), ...faces.map(f => path.join(fonts, f[2]))
    ].map(f => [path.relative(root, f), sha(fs.readFileSync(f))]));
    const entries = [];
    for (const scale of [1, 2]) {
      const context = await browser.newContext({ viewport: { width: 1440, height: 900 }, deviceScaleFactor: scale,
        locale: "en-US", timezoneId: "UTC", colorScheme: "light", reducedMotion: "reduce", serviceWorkers: "block" });
      const requests = [];
      await context.route("**/*", route => {
        if (!route.request().url().startsWith(`${base}/`)) { requests.push(route.request().url()); return route.abort(); }
        return route.continue();
      });
      for (const name of names) {
        const page = await context.newPage();
        const errors = [];
        page.on("pageerror", error => errors.push(String(error)));
        page.on("response", response => { if (!response.ok()) errors.push(`HTTP ${response.status()} ${response.url()}`); });
        await page.goto(`${base}/${name}.dc.html`, { waitUntil: "networkidle" });
        await page.waitForFunction(() => document.documentElement.dataset.dcResolved !== undefined);
        const state = await page.evaluate(async () => {
          await document.fonts.ready;
          await Promise.all(Array.from(document.fonts).map(face => face.load()));
          const root = document.querySelector("x-dc");
          if (!root) return { error: document.documentElement.dataset.dcError || "Missing resolved board root" };
          const rect = root.getBoundingClientRect();
          const elements = Array.from(root.querySelectorAll("*"));
          return { error: document.documentElement.dataset.dcError,
            width: rect.width, height: rect.height,
            unresolved: /\{\{/.test(root.innerHTML), directives: root.querySelectorAll("sc-for,sc-if,template[data-dc-directive],dc-import").length,
            invalidSvg: elements.filter(e => ["rect", "path", "circle", "g", "line", "text", "polygon"].includes(e.localName) && e.namespaceURI !== "http://www.w3.org/2000/svg").length,
            text: root.innerText, fonts: Array.from(document.fonts).map(f => ({ family: f.family, weight: f.weight, status: f.status })) };
        });
        if (state.error || errors.length || requests.length || state.unresolved || state.directives || state.invalidSvg || state.width !== 1440 || state.height !== 900) {
          throw new Error(`${name}@${scale}: ${JSON.stringify({ ...state, text: undefined, errors, requests })}`);
        }
        if (state.fonts.length !== faces.length || state.fonts.some(f => f.status !== "loaded")) throw new Error(`${name}: fonts not loaded`);
        const bytes = await page.screenshot({ animations: "disabled", scale: "device" });
        const repeat = await page.screenshot({ animations: "disabled", scale: "device" });
        if (!bytes.equals(repeat)) throw new Error(`${name}@${scale}: repeated capture differs`);
        const png = `${name}-${scale}x.png`;
        const target = path.join(output, png);
        if (verify) {
          if (!fs.existsSync(target) || !bytes.equals(fs.readFileSync(target))) throw new Error(`${png}: committed reference differs`);
        } else { fs.mkdirSync(output, { recursive: true }); fs.writeFileSync(target, bytes); }
        entries.push({ name, scale, png, width: bytes.readUInt32BE(16), height: bytes.readUInt32BE(20), sha256: sha(bytes), text_sha256: sha(state.text), fonts: state.fonts });
        console.log(`${verify ? "Verified" : "Rendered"} ${png} ${bytes.length} bytes`);
        await page.close();
      }
      await context.close();
    }
    const manifest = { schema: 1, purpose: "Design references only; no JavaFX parity sign-off implied", browser: { playwrightCore: "1.49.1", chromium: browserVersion, revision: 1148 },
      platform: { os: os.platform(), arch: os.arch(), node: process.version }, viewport: { width: 1440, height: 900 },
      network: "loopback only; external requests refused", inputs: assets, entries };
    const target = path.join(output, "manifest.json");
    if (verify) {
      const prior = JSON.parse(fs.readFileSync(target, "utf8"));
      if (JSON.stringify(prior) !== JSON.stringify(manifest)) throw new Error("Reference manifest differs (including platform/browser/input identity)");
    } else fs.writeFileSync(target, JSON.stringify(manifest, null, 2) + "\n");
  } finally { if (browser) await browser.close(); await new Promise(resolve => server.close(resolve)); }
}

main().catch(error => { console.error(error); process.exitCode = 1; });
