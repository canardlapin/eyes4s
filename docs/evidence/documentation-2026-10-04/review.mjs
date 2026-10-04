// Rendered review of the built public site (docs/tlSite output) in an owned headless Chromium.
//
//   node docs/evidence/documentation-2026-10-04/review.mjs SITE_DIR OUT_DIR
//
// Serves SITE_DIR on a loopback port, opens every page at 1440x1000 and 390x844, records the
// title, heading, navigation, page and code-block overflow, console and page errors and failed
// local requests, exercises the mobile navigation, checks every local link and anchor and every
// repository link against this checkout, writes OUT_DIR/receipt.json and a screenshot per page
// and viewport, then closes the browser and the server. It uses Playwright's own Chromium build
// (resolved from the user's Node modules), never an installed system browser.
import { chromium } from "playwright";
import { createServer } from "node:http";
import { execFileSync } from "node:child_process";
import { existsSync, mkdirSync, readdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, extname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const [site, out] = process.argv.slice(2).map((p) => resolve(p));
const repo = resolve(dirname(fileURLToPath(import.meta.url)), "../../..");
const repoPrefix = "https://github.com/canardlapin/eyes4s/blob/main/";
const viewports = [
  [1440, 1000],
  [390, 844],
];
const pages = readdirSync(site).filter((f) => f.endsWith(".html")).sort();
const types = { ".html": "text/html", ".css": "text/css", ".js": "text/javascript", ".svg": "image/svg+xml", ".png": "image/png", ".woff": "font/woff", ".woff2": "font/woff2", ".ttf": "font/ttf", ".json": "application/json" };

const server = createServer((req, res) => {
  const path = join(site, decodeURIComponent(new URL(req.url, "http://x").pathname));
  if (!path.startsWith(site) || !existsSync(path) || !path.includes(".")) {
    res.writeHead(404).end();
    return;
  }
  res.writeHead(200, { "content-type": types[extname(path)] ?? "application/octet-stream" }).end(readFileSync(path));
});
await new Promise((ok) => server.listen(0, "127.0.0.1", ok));
const base = `http://127.0.0.1:${server.address().port}/`;

function descendants(pid) {
  const children = new Map();
  for (const line of execFileSync("ps", ["-axo", "pid=,ppid="]).toString().trim().split("\n")) {
    const [child, parent] = line.trim().split(/\s+/).map(Number);
    if (!children.has(parent)) children.set(parent, []);
    children.get(parent).push(child);
  }
  const found = [];
  const todo = [pid];
  while (todo.length) for (const c of children.get(todo.pop()) ?? []) found.push(c), todo.push(c);
  return found;
}

// Helium scrolls #container, not the document, so horizontal overflow is measured there:
// the container's scroll width against its client width, and the innermost elements that
// extend past it without a scrolling ancestor of their own.
const overflowProbe = () => {
  const c = document.querySelector("#container") ?? document.documentElement;
  const cw = c.clientWidth;
  const edge = c.getBoundingClientRect().left + cw + 1;
  const leaves = [];
  for (const el of c.querySelectorAll("*")) {
    const r = el.getBoundingClientRect();
    if (r.right <= edge || [...el.children].some((k) => k.getBoundingClientRect().right > edge)) continue;
    let scroller = false;
    for (let q = el.parentElement; q && q !== c; q = q.parentElement) {
      const o = getComputedStyle(q).overflowX;
      if (o === "auto" || o === "scroll" || o === "hidden") {
        scroller = true;
        break;
      }
    }
    if (!scroller) leaves.push({ tag: el.tagName, text: (el.textContent ?? "").slice(0, 80), right: Math.round(r.right) });
  }
  const code = [...c.querySelectorAll("pre")]
    .map((pre) => ({ scrollWidth: pre.scrollWidth, clientWidth: pre.clientWidth, overflowX: getComputedStyle(pre).overflowX }))
    .filter((x) => x.scrollWidth > x.clientWidth + 1);
  return { containerScrollWidth: c.scrollWidth, containerWidth: cw, out: leaves.slice(0, 20), code };
};

// For a full-length screenshot only, after measuring: let the document scroll instead of #container.
const unclip = () => {
  for (const el of [document.documentElement, document.body]) el.style.height = "auto";
  const c = document.querySelector("#container");
  if (c) Object.assign(c.style, { position: "static", overflow: "visible", height: "auto" });
};

mkdirSync(out, { recursive: true });
const receipt = { ownerPid: process.pid, base: "loopback", pages: [], screenshots: [] };
const browser = await chromium.launch({ headless: true });
receipt.browser = { name: "chromium", version: browser.version() };
receipt.browserPids = descendants(process.pid);
const links = new Set();
try {
  for (const [width, height] of viewports) {
    const context = await browser.newContext({ viewport: { width, height } });
    for (const name of pages) {
      const page = await context.newPage();
      const problems = [];
      page.on("console", (m) => m.type() === "error" && problems.push(`console error: ${m.text()}`));
      page.on("pageerror", (e) => problems.push(`pageerror: ${e.message}`));
      page.on("requestfailed", (r) => r.url().startsWith(base) && problems.push(`failed: ${r.url()}`));
      page.on("response", (r) => r.url().startsWith(base) && r.status() >= 400 && problems.push(`HTTP ${r.status()}: ${r.url()}`));
      await page.goto(base + name, { waitUntil: "load" });
      const facts = await page.evaluate(() => ({
        title: document.title,
        h1: document.querySelector("h1")?.textContent.trim() ?? null,
        scrollWidth: document.documentElement.scrollWidth,
        width: document.documentElement.clientWidth,
        nav: [...document.querySelectorAll("#sidebar a")].map((a) => ({ text: a.textContent.trim(), href: a.getAttribute("href") })),
        hrefs: [...document.querySelectorAll("a[href]")].map((a) => a.getAttribute("href")),
      }));
      for (const href of facts.hrefs) links.add(JSON.stringify([name, href]));
      delete facts.hrefs;
      const overflow = await page.evaluate(overflowProbe);
      let mobile = null;
      if (width < 1000) {
        // Visible means rendered and horizontally on screen: the closed mobile sidebar
        // stays in the layout but is moved off canvas.
        const onScreen = () =>
          page.evaluate(() => {
            const s = document.querySelector("#sidebar");
            const r = s.getBoundingClientRect();
            const st = getComputedStyle(s);
            return st.display !== "none" && st.visibility !== "hidden" && r.width > 0 && r.right > 1 && r.left < document.documentElement.clientWidth - 1;
          });
        const before = await onScreen();
        await page.click("#nav-icon");
        await page.waitForTimeout(600);
        const after = await onScreen();
        let navigated = null;
        const target = page.locator(name === "reference.html" ? "#sidebar a[href='templates.html']" : "#sidebar a[href='reference.html']").first();
        if (after && (await target.count())) {
          await target.click();
          await page.waitForLoadState("load");
          navigated = page.url().endsWith(name === "reference.html" ? "templates.html" : "reference.html");
          await page.goto(base + name, { waitUntil: "load" });
        }
        mobile = { sidebarBefore: before, sidebarAfterToggle: after, navigatedFromSidebar: navigated };
      }
      const shot = `${width}-${name}.png`;
      await page.evaluate(unclip);
      await page.screenshot({ path: join(out, shot), fullPage: true });
      receipt.screenshots.push(shot);
      receipt.pages.push({ page: name, viewport: { width, height }, ...facts, containerScrollWidth: overflow.containerScrollWidth, containerWidth: overflow.containerWidth, overflow: overflow.out, codeOverflow: overflow.code, problems, mobileNavigation: mobile });
      await page.close();
    }
    await context.close();
  }
  receipt.links = await checkLinks([...links].map((l) => JSON.parse(l)));
} finally {
  await browser.close();
  await new Promise((ok) => server.close(ok));
}
receipt.closed = true;
receipt.browserPidsAlive = receipt.browserPids.filter((pid) => {
  try {
    process.kill(pid, 0);
    return true;
  } catch {
    return false;
  }
});
receipt.horizontalOverflow = receipt.pages.filter((p) => p.containerScrollWidth > p.containerWidth + 1 || p.scrollWidth > p.width + 1 || p.overflow.length).map((p) => `${p.viewport.width} ${p.page}`);
receipt.errors = receipt.pages.reduce((n, p) => n + p.problems.length, 0) + receipt.links.broken.length + receipt.horizontalOverflow.length;
writeFileSync(join(out, "receipt.json"), JSON.stringify(receipt, null, 2) + "\n");
console.log(JSON.stringify({ pages: receipt.pages.length, errors: receipt.errors, overflow: receipt.horizontalOverflow, closed: receipt.closed, alive: receipt.browserPidsAlive.length }));

async function checkLinks(pairs) {
  const broken = [];
  const external = new Set();
  let local = 0;
  let repository = 0;
  const anchors = new Map();
  const context = await browser.newContext();
  const page = await context.newPage();
  for (const [source, href] of pairs.sort()) {
    if (href.startsWith(repoPrefix)) {
      repository++;
      const path = new URL(href).pathname.replace("/canardlapin/eyes4s/blob/main/", "");
      if (!existsSync(join(repo, path))) broken.push({ source, href, reason: "no such file in this checkout" });
      continue;
    }
    if (/^(https?|mailto):/.test(href)) {
      external.add(href);
      continue;
    }
    local++;
    const url = new URL(href, base + source);
    const target = url.pathname.slice(1) || "index.html";
    if (!existsSync(join(site, target))) {
      broken.push({ source, href, reason: "no such page or asset" });
      continue;
    }
    const fragment = decodeURIComponent(url.hash.slice(1));
    if (fragment) {
      if (!anchors.has(target)) {
        await page.goto(base + target, { waitUntil: "load" });
        anchors.set(target, new Set(await page.evaluate(() => [...document.querySelectorAll("[id],[name]")].map((e) => e.id || e.getAttribute("name")))));
      }
      if (!anchors.get(target).has(fragment)) broken.push({ source, href, reason: "no such anchor" });
    }
  }
  await context.close();
  return { local, repository, external: [...external].sort(), broken };
}
