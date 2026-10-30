// node parity.js <golden-dir> <native-dir> page...  → per pane PASS/FAIL; exit 1 on any FAIL.
// Thresholds from spike/RESULT.md (JavaFX 24 vs Chromium on identical geometry):
//   non-text pixels per pane: mean Δ ≤ 2.0 and pixels Δ>48 ≤ 0.5%;
//   flat colour (golden pixel equal to both neighbours along a row or a column: fills, borders, hairlines):
//   native within Δ 3 on ≥ 99.8% of them (catches a wrong token that moves too few pixels to shift the mean);
//   text runs: ink box left edge exact, other edges within ±1 px, mean ink darkness within ±8% (pixels inside text boxes are excluded above).
const { chromium } = require(process.env.PLAYWRIGHT_MODULE); const fs = require('fs'), path = require('path');
const T = { meanDelta: 2.0, over48: 0.005, inkPx: 1, inkDark: 0.08, flatDelta: 3, flatMiss: 0.002 };
(async () => {
  const [gdir, ndir, ...pages] = process.argv.slice(2); let fails = 0;
  const b = await chromium.launch(); const p = await b.newPage();
  for (const pg of pages) {
    const reg = JSON.parse(fs.readFileSync(path.join(gdir, pg + '.regions.json')));
    const res = await p.evaluate(async ({ G, N, reg, T }) => {
      const load = s => new Promise(r => { const i = new Image(); i.onload = () => r(i); i.src = s; });
      const px = async s => { const i = await load(s); const c = document.createElement('canvas'); c.width = i.width; c.height = i.height; const g = c.getContext('2d'); g.drawImage(i, 0, 0); return g.getImageData(0, 0, i.width, i.height); };
      const g = await px(G), n = await px(N); if (g.width !== n.width || g.height !== n.height) return { error: `size ${g.width}x${g.height} vs ${n.width}x${n.height}` };
      const W = g.width, H = g.height, inText = new Uint8Array(W * H);
      for (const t of reg.text) { const [x0, y0, x1, y1] = t.rect; for (let y = Math.max(0, y0 - 2); y < Math.min(H, y1 + 2); y++) for (let x = Math.max(0, x0 - 2); x < Math.min(W, x1 + 2); x++) inText[y * W + x] = 1; }
      const d = k => Math.max(Math.abs(g.data[k] - n.data[k]), Math.abs(g.data[k + 1] - n.data[k + 1]), Math.abs(g.data[k + 2] - n.data[k + 2]));
      const same = (i, j) => g.data[i] === g.data[j] && g.data[i + 1] === g.data[j + 1] && g.data[i + 2] === g.data[j + 2];
      const flat = (x, y) => { const k = (y * W + x) * 4; return (x > 0 && x < W - 1 && same(k, k - 4) && same(k, k + 4)) || (y > 0 && y < H - 1 && same(k, k - 4 * W) && same(k, k + 4 * W)); };
      const paneList = reg.panes.length ? reg.panes : [{ name: 'page', rect: [0, 0, W, H] }];
      const panes = paneList.map(pn => { const [x0, y0, x1, y1] = pn.rect; let s = 0, c = 0, o = 0, fc = 0, fm = 0; for (let y = Math.max(0, y0); y < Math.min(H, y1); y++) for (let x = Math.max(0, x0); x < Math.min(W, x1); x++) { const i = y * W + x; if (inText[i]) continue; const v = d(i * 4); s += v; c++; if (v > 48) o++; if (flat(x, y)) { fc++; if (v > T.flatDelta) fm++; } } const mean = c ? s / c : 0, ov = c ? o / c : 0, fmiss = fc ? fm / fc : 0; return { name: pn.name, mean: +mean.toFixed(2), over48: +(100 * ov).toFixed(2), flatMiss: +(100 * fmiss).toFixed(2), ok: mean <= T.meanDelta && ov <= T.over48 && fmiss <= T.flatMiss }; });
      const ink = (im, rect) => { const [x0, y0, x1, y1] = rect; // background = median-ish of the box border
        const bg = []; for (let x = x0 - 2; x < x1 + 2; x++) for (const y of [y0 - 2, y1 + 1]) if (x >= 0 && x < W && y >= 0 && y < H) { const k = (y * W + x) * 4; bg.push(im.data[k] + im.data[k + 1] + im.data[k + 2]); }
        bg.sort((a, b) => a - b); const b0 = bg[bg.length >> 1] / 3; let l = 1e9, r = -1, t = 1e9, bt = -1, sum = 0, cnt = 0;
        for (let y = Math.max(0, y0 - 2); y < Math.min(H, y1 + 2); y++) for (let x = Math.max(0, x0 - 2); x < Math.min(W, x1 + 2); x++) { const k = (y * W + x) * 4, v = Math.abs((im.data[k] + im.data[k + 1] + im.data[k + 2]) / 3 - b0); if (v > 60) { l = Math.min(l, x); r = Math.max(r, x); t = Math.min(t, y); bt = Math.max(bt, y); } if (v > 8) { sum += v; cnt++; } }
        return { box: [l, t, r, bt], dark: cnt ? sum / cnt : 0 }; };
      const text = []; for (const tr of reg.text) { const a = ink(g, tr.rect), c = ink(n, tr.rect); if (a.box[2] < 0) continue;
        const dx = a.box[0] !== c.box[0] ? 99 : Math.max(...a.box.slice(1).map((v, i) => Math.abs(v - c.box[i + 1]))), dk = a.dark ? Math.abs(a.dark - c.dark) / a.dark : 0;
        text.push({ t: tr.t, dx, dk: +dk.toFixed(3), ok: dx <= T.inkPx && dk <= T.inkDark }); }
      return { panes, text };
    }, { G: 'data:image/png;base64,' + fs.readFileSync(path.join(gdir, pg + '.png')).toString('base64'), N: 'data:image/png;base64,' + fs.readFileSync(path.join(ndir, pg + '.png')).toString('base64'), reg, T });
    if (res.error) { console.log(pg, 'size -> FAIL', res.error); fails++; continue; }
    for (const pn of res.panes) { console.log(`${pg} pane ${pn.name} meanΔ ${pn.mean} >48 ${pn.over48}% flat-miss ${pn.flatMiss}% -> ${pn.ok ? 'PASS' : 'FAIL'}`); if (!pn.ok) fails++; }
    const bad = res.text.filter(t => !t.ok);
    console.log(`${pg} text runs ${res.text.length - bad.length}/${res.text.length} (left exact, ±${T.inkPx}px, ±${T.inkDark * 100}%) -> ${bad.length ? 'FAIL ' + JSON.stringify(bad.slice(0, 4)) : 'PASS'}`); if (bad.length) fails++;
  }
  await b.close(); console.log(fails ? `PARITY FAIL (${fails})` : 'PARITY PASS'); process.exit(fails ? 1 : 0);
})();
