// node golden.js <build-dir> <out-dir> page...  → <page>.png (1440x900 @1x) + <page>.regions.json (panes, text runs)
const { chromium } = require(process.env.PLAYWRIGHT_MODULE); const fs = require('fs'), path = require('path');
(async () => {
  const [dir, out, ...pages] = process.argv.slice(2); fs.mkdirSync(out, { recursive: true });
  const b = await chromium.launch(); const [VW, VH] = (process.env.VIEWPORT || '1440x900').split('x').map(Number); const ctx = await b.newContext({ viewport: { width: VW, height: VH }, deviceScaleFactor: 1 });
  for (const pg of pages) {
    const p = await ctx.newPage(); await p.goto('file://' + path.resolve(dir, pg + '.html')); await p.waitForTimeout(500);
    await p.screenshot({ path: path.join(out, pg + '.png') });
    const regions = await p.evaluate(() => {
      const r = e => { const b = e.getBoundingClientRect(); return [Math.round(b.left), Math.round(b.top), Math.round(b.right), Math.round(b.bottom)]; };
      const panes = [...document.querySelectorAll('.toolbar, .trail, .statusbar, .pane')].map((e, i) => ({ name: (e.className.split(' ')[0]) + '#' + i + ':' + ((e.querySelector('.title') || {}).textContent || '').slice(0, 24), rect: r(e) }));
      const text = [];
      const walk = n => { for (const c of n.childNodes) { if (c.nodeType === 3 && c.textContent.trim()) { const range = document.createRange(); range.selectNodeContents(c); for (const q of range.getClientRects()) if (q.width > 2 && q.height > 4) text.push({ t: c.textContent.trim().slice(0, 40), rect: [Math.floor(q.left), Math.floor(q.top), Math.ceil(q.right), Math.ceil(q.bottom)] }); } else if (c.nodeType === 1) walk(c); } };
      walk(document.body);
      return { panes, text };
    });
    fs.writeFileSync(path.join(out, pg + '.regions.json'), JSON.stringify(regions));
    console.log(pg, 'panes', regions.panes.length, 'text runs', regions.text.length);
    await p.close();
  }
  await b.close();
})();
