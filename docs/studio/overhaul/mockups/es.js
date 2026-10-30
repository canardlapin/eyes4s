// Eyes Studio redesign kit: shell, icons, trial stage, small plots.
// Stages and plots are SVG/canvas standing in for Intaglio scenes: straight segments,
// circles, polygons, text, and one smoothed raster per map (JavaFX ImageView smooth=true).
(function () {
  const ES = (window.ES = {});
  const F = window.FX;

  // ── icons (16px stroke, SVGPath-compatible) ──────────────────
  const I = (d, extra = '') =>
    `<svg width="16" height="16" viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true" ${extra}>${d}</svg>`;
  ES.icon = {
    back: I('<path d="M10 3 5 8l5 5"/>'),
    fwd: I('<path d="m6 3 5 5-5 5"/>'),
    chev: I('<path d="m6 4 4 4-4 4"/>'),
    down: I('<path d="m4 6 4 4 4-4"/>'),
    search: I('<circle cx="7" cy="7" r="4.5"/><path d="m10.5 10.5 3 3"/>'),
    info: I('<circle cx="8" cy="8" r="6"/><path d="M8 7.2v4M8 4.9v.1"/>'),
    more: I('<path d="M3.5 8h.01M8 8h.01M12.5 8h.01" stroke-width="2.2"/>'),
    table: I('<rect x="2.5" y="3" width="11" height="10" rx="1"/><path d="M2.5 6.5h11M2.5 9.8h11M6.5 6.5V13"/>'),
    chart: I('<path d="M2.5 13.5h11M4.5 11V8M8 11V4.5M11.5 11V6.5"/>'),
    expand: I('<path d="M9.5 2.5h4v4M6.5 13.5h-4v-4M13.5 2.5 9 7M2.5 13.5 7 9"/>'),
    popout: I('<path d="M9 2.5h4.5V7M13.5 2.5 8 8"/><path d="M11.5 9.5v4h-9v-9h4"/>'),
    side: I('<rect x="2.5" y="3" width="11" height="10" rx="1"/><path d="M10 3v10"/>'),
    jobs: I('<path d="M3 4.5h10M3 8h10M3 11.5h6"/>'),
    warn: I('<path d="M8 2.5 14 13H2z"/><path d="M8 6.5v3M8 11.2v.1"/>'),
    play: I('<path d="M5 3.5v9l7-4.5z"/>'),
    step: I('<path d="M4 3.5v9M7 3.5v9l6-4.5z"/>'),
    stepb: I('<path d="M12 3.5v9M9 3.5v9l-6-4.5z"/>'),
    plus: I('<path d="M8 3v10M3 8h10"/>'),
    file: I('<path d="M4 2.5h5l3 3v8H4z"/><path d="M9 2.5v3h3"/>'),
    image: I('<rect x="2.5" y="3" width="11" height="10" rx="1"/><path d="m3 11 3-3 3 3 2-2 2 2"/>'),
    folder: I('<path d="M2.5 4.5h4l1.5 1.5h5.5v6.5h-11z"/>'),
    check: I('<path d="m3.5 8.5 3 3 6-7"/>'),
    x: I('<path d="m4 4 8 8M12 4l-8 8"/>'),
  };
  ES.draftmark = `<svg class="draftmark" width="9" height="9" viewBox="0 0 9 9" aria-hidden="true"><rect x="0.75" y="0.75" width="7.5" height="7.5" rx="1.5" fill="none" stroke="var(--ink-3)" stroke-width="1.5" stroke-dasharray="2 1.6"/></svg>`;
  ES.mark = `<svg class="mark" viewBox="0 0 20 20" fill="none" stroke="currentColor" stroke-width="1.6" aria-hidden="true"><path d="M1.8 10C4 5.8 6.8 4 10 4s6 1.8 8.2 6c-2.2 4.2-5 6-8.2 6s-6-1.8-8.2-6Z"/><circle cx="10" cy="10" r="2.6" fill="currentColor" stroke="none"/></svg>`;

  // role glyphs as tiny inline svgs
  ES.glyph = {
    query: (s = 10) => `<svg width="${s}" height="${s}" viewBox="0 0 10 10" aria-hidden="true"><circle cx="5" cy="5" r="4.2" fill="var(--query)"/></svg>`,
    match: (s = 10) => `<svg width="${s}" height="${s}" viewBox="0 0 10 10" aria-hidden="true"><path d="M5 .6 9.4 5 5 9.4.6 5z" fill="var(--match)"/></svg>`,
    control: (s = 10) => `<svg width="${s}" height="${s}" viewBox="0 0 10 10" aria-hidden="true"><circle cx="5" cy="5" r="3.6" fill="none" stroke="var(--control)" stroke-width="1.6"/></svg>`,
  };

  // ── shell ────────────────────────────────────────────────────
  const PERSP = ['Data', 'Analysis', 'Results', 'Report'];
  ES.shell = function (o) {
    const persp = PERSP.map(p => `<button class="${p === o.persp ? 'on' : ''}" title="${p} ⌘${PERSP.indexOf(p) + 1}">${p}</button>`).join('');
    const trail = o.trail.map((c, i) => {
      const last = i === o.trail.length - 1;
      return last ? `<span class="crumb cur" aria-current="page">${c}</span>` : `<button class="crumb">${c}</button><span class="sep" aria-hidden="true">/</span>`;
    }).join('');
    const state = o.state || `<button class="state" aria-haspopup="menu" title="Study state: run, dataset, analysis draft, jobs"><span class="part"><span class="dot"></span>Run 7 · current</span><span class="sep"></span><span class="part muted">Data r3</span><span class="sep"></span><span class="part">${ES.draftmark}Analysis rev 5 · not run</span><span class="chev" style="display:flex">${ES.icon.down}</span></button>`;
    return `
<div class="app">
  <div class="toolbar row">
    <div class="brand row">${ES.mark}<button class="project row">memory-study<span class="chev">${ES.icon.down}</span></button></div>
    <div class="persp-wrap"><div class="persp row">${persp}</div></div>
    <div class="status-cluster row">${state}</div>
  </div>
  <div class="trail row">
    <button class="ibtn" aria-label="Back">${ES.icon.back}</button><button class="ibtn" aria-label="Forward">${ES.icon.fwd}</button>
    <div class="row" style="gap:2px;margin-left:4px">${trail}</div>
    <div class="grow"></div>
    ${o.notice ? `<div class="notice row">${o.notice}</div>` : ''}
  </div>
  <div class="work" id="work">${o.work}</div>
  <div class="statusbar row"><span>${o.statusHint || ''}</span><span class="grow"></span><span>Saved 10:24</span></div>
</div>`;
  };
  ES.mount = function (o) {
    document.body.innerHTML = ES.shell(o);
    if (o.dark) document.documentElement.classList.add('dark');
    // keyboard: one tab stop per list (arrows inside), every link and tile is a button
    document.querySelectorAll('.list').forEach(l => { if (!l.hasAttribute('tabindex')) l.tabIndex = 0; l.setAttribute('role', 'listbox'); });
    document.querySelectorAll('.link, .tile').forEach(l => { l.tabIndex = 0; l.setAttribute('role', 'button'); });
  };

  // ── map recipe (desktop MapColours / Isolines) ───────────────
  // v4 ramp (owner decision 2026-10-29): every stop darker than the #808080 ground (L* 51.8 → 16.4 vs 53.6),
  // so low mass fades by transparency, never by near-white; darker always means more.
  const RAMP = [[0xB0, 0x60, 0x9C], [0x8E, 0x3E, 0x7E], [0x6E, 0x23, 0x66], [0x4A, 0x0C, 0x47]];
  ES.FLOOR = 0.25; ES.MAP_OPACITY = 0.85;
  ES.ramp = function (t) {
    t = Math.max(0, Math.min(1, t));
    const seg = RAMP.length - 1, at = t * seg, k = Math.min(seg - 1, Math.floor(at)), f = at - k;
    return [0, 1, 2].map(i => Math.round(RAMP[k][i] + (RAMP[k + 1][i] - RAMP[k][i]) * f));
  };
  const GW = 64, GH = 48, FW = 1024, FH = 768, FX0 = 448, FY0 = 156;
  ES.fixations = function (key) {
    return F.fix[key].map(r => ({ ord: r[0], sx: r[1], sy: r[2], x: r[1] - FX0, y: r[2] - FY0, onset: r[3], dur: r[4], n: r[5], inWin: r[6] }));
  };
  ES.grid = function (fix, sigmaDeg) {
    const s = sigmaDeg * 35, cw = FW / GW, ch = FH / GH, g = [];
    let total = 0, max = 0;
    for (let j = 0; j < GH; j++) {
      const row = [];
      for (let i = 0; i < GW; i++) {
        const cx = (i + 0.5) * cw, cy = (j + 0.5) * ch;
        let v = 0;
        for (const f of fix) if (f.inWin) { const dx = cx - f.x, dy = cy - f.y; v += f.dur * Math.exp(-(dx * dx + dy * dy) / (2 * s * s)); }
        row.push(v); total += v; if (v > max) max = v;
      }
      g.push(row);
    }
    return { g, total, max };
  };
  function levelFor(G, share) {
    const sorted = G.g.flat().sort((a, b) => b - a);
    let acc = 0;
    for (const v of sorted) { acc += v; if (acc >= share * G.total) return v; }
    return 0;
  }
  function contour(G, level, sx, sy) {
    const g = G.g, P = (i, j) => [(i + 0.5) * sx, (j + 0.5) * sy];
    const lerp = (p, q, v0, v1) => { const t = v1 === v0 ? 0.5 : (level - v0) / (v1 - v0); return [p[0] + (q[0] - p[0]) * t, p[1] + (q[1] - p[1]) * t]; };
    let d = '';
    const seg = (p, q) => { d += 'M' + p[0].toFixed(1) + ' ' + p[1].toFixed(1) + 'L' + q[0].toFixed(1) + ' ' + q[1].toFixed(1); };
    for (let j = 0; j < GH - 1; j++) for (let i = 0; i < GW - 1; i++) {
      const a = g[j][i], b = g[j][i + 1], c = g[j + 1][i + 1], e = g[j + 1][i];
      const A = P(i, j), B = P(i + 1, j), C = P(i + 1, j + 1), E = P(i, j + 1);
      const code = (a >= level ? 8 : 0) | (b >= level ? 4 : 0) | (c >= level ? 2 : 0) | (e >= level ? 1 : 0);
      if (code === 0 || code === 15) continue;
      const top = lerp(A, B, a, b), right = lerp(B, C, b, c), bottom = lerp(E, C, e, c), left = lerp(A, E, a, e);
      const mean = (a + b + c + e) / 4 >= level;
      switch (code) {
        case 1: case 14: seg(left, bottom); break;
        case 2: case 13: seg(bottom, right); break;
        case 3: case 12: seg(left, right); break;
        case 4: case 11: seg(top, right); break;
        case 6: case 9: seg(top, bottom); break;
        case 7: case 8: seg(left, top); break;
        case 5: if (mean) { seg(left, top); seg(bottom, right); } else { seg(left, bottom); seg(top, right); } break;
        case 10: if (mean) { seg(left, bottom); seg(top, right); } else { seg(left, top); seg(bottom, right); } break;
      }
    }
    return d;
  }
  // 64x48 raster: ramp colour, alpha floor at 10% of the limit, global opacity .6
  ES.mapCanvas = function (G, lim) {
    const c = document.createElement('canvas');
    c.width = GW; c.height = GH;
    const ctx = c.getContext('2d'), im = ctx.createImageData(GW, GH);
    for (let j = 0; j < GH; j++) for (let i = 0; i < GW; i++) {
      const t = G.g[j][i] / lim, [r, g, b] = ES.ramp(t), a = Math.min(1, t / ES.FLOOR) * ES.MAP_OPACITY, k = (j * GW + i) * 4;
      im.data[k] = r; im.data[k + 1] = g; im.data[k + 2] = b; im.data[k + 3] = Math.round(a * 255);
    }
    ctx.putImageData(im, 0, 0);
    return c.toDataURL();
  };

  // ── trial stage ──────────────────────────────────────────────
  // o: {key, role:'query'|'match'|'control'|'neutral', display:'blank'|'image', image, w, h,
  //     layers:{map,order,points}, sigma, lim, label, selected (ordinal), underlay}
  ES.stage = function (o) {
    const fix = ES.fixations(o.key);
    // crop: image frame plus every fixation, padded; keep aspect of the box
    let x0 = FX0, y0 = FY0, x1 = FX0 + FW, y1 = FY0 + FH;
    for (const f of fix) { x0 = Math.min(x0, f.sx); y0 = Math.min(y0, f.sy); x1 = Math.max(x1, f.sx); y1 = Math.max(y1, f.sy); }
    const pad = 36; x0 -= pad; y0 -= pad; x1 += pad; y1 += pad;
    if (o.frameOnly) { x0 = FX0; y0 = FY0; x1 = FX0 + FW; y1 = FY0 + FH; }
    if (o.crop) [x0, y0, x1, y1] = o.crop;
    const k = Math.min(o.w / (x1 - x0), o.h / (y1 - y0));
    const ox = (o.w - (x1 - x0) * k) / 2 - x0 * k, oy = (o.h - (y1 - y0) * k) / 2 - y0 * k;
    const X = sx => ox + sx * k, Y = sy => oy + sy * k;
    const fx = X(FX0), fy = Y(FY0), fw = FW * k, fh = FH * k;
    let s = `<svg width="${o.w}" height="${o.h}" viewBox="0 0 ${o.w} ${o.h}" style="display:block">`;
    s += `<rect width="${o.w}" height="${o.h}" fill="${o.ground || 'var(--stage)'}"/>`;
    if (o.display === 'image' || o.underlay) s += `<image href="${o.image}" x="${fx}" y="${fy}" width="${fw}" height="${fh}" preserveAspectRatio="none" style="opacity:${o.underlay ? .35 : 'var(--image-opacity, 1)'};${o.layers.map || o.layers.points ? 'filter:grayscale(1) contrast(.13) brightness(1.23)' : ''}"/>`;
    else s += `<rect x="${fx}" y="${fy}" width="${fw}" height="${fh}" fill="${o.frameFill || '#808080'}"/>`;
    const G = ES.grid(fix, o.sigma || 2), lim = o.lim || G.max;
    if (o.layers.map) {
      s += `<image href="${ES.mapCanvas(G, lim)}" x="${fx}" y="${fy}" width="${fw}" height="${fh}" preserveAspectRatio="none" style="image-rendering:auto"/>`;
      const sx = fw / GW, sy = fh / GH;
      for (const [share, wt] of (o.noIso ? [] : o.frameOnly ? [[0.5, 1]] : [[0.9, 1], [0.5, 1.5]])) {
        const d = contour(G, levelFor(G, share), sx, sy);
        s += `<g transform="translate(${fx} ${fy})"><path d="${d}" fill="none" stroke="rgba(255,255,255,.6)" stroke-width="${wt + 1.25}" stroke-linecap="round"/><path d="${d}" fill="none" stroke="rgba(28,30,33,.9)" stroke-width="${wt}" stroke-linecap="round"/></g>`;
      }
    }
    // analysis window outline (image frame)
    if (!o.frameOnly) s += `<rect x="${fx + 0.5}" y="${fy + 0.5}" width="${fw - 1}" height="${fh - 1}" fill="none" stroke="var(--stage-ink)" stroke-opacity=".6" stroke-width="1"/>`;
    if (o.layers.order) {
      // runs of in-window fixations are one path; a step to or from an outside-window
      // fixation stops at the window edge with a 4 px tick (no line is drawn outside)
      const inside = f => f.inWin, edge = (a, b) => {
        let t = 1;
        for (const [p0, p1, lo, hi] of [[a.sx, b.sx, FX0, FX0 + FW], [a.sy, b.sy, FY0, FY0 + FH]]) {
          const d = p1 - p0; if (!d) continue;
          for (const bound of [lo, hi]) { const u = (bound - p0) / d; if (u > 0 && u < t) t = u; }
        }
        return { x: a.sx + (b.sx - a.sx) * t, y: a.sy + (b.sy - a.sy) * t, horiz: Math.abs(a.sx + (b.sx - a.sx) * t - FX0) < 0.5 || Math.abs(a.sx + (b.sx - a.sx) * t - FX0 - FW) < 0.5 };
      };
      // each step is one segment that starts and ends at the marker edges (no barbs inside markers);
      // a step to or from an outside-window fixation stops at the window edge with its own 6 px tick
      const rad = f => o.layers.points ? Math.sqrt(f.dur / 1000) * 13 * Math.max(0.55, k / 0.32) + 1.5 : 0;
      // a segment keeps only its part outside every marker, by each marker's real shape
      // (circle: Euclidean radius; matched diamond: L1 radius 1.25 r), with a 1.5 px gap
      const marks = o.layers.points ? fix.map(f => ({ x: X(f.sx), y: Y(f.sy), r: rad(f) - 1.5, diamond: f.inWin && o.role === 'match' })) : [];
      const covered = (x, y) => marks.some(m => m.diamond ? Math.abs(x - m.x) + Math.abs(y - m.y) < m.r * 1.25 + 1.5 : Math.hypot(x - m.x, y - m.y) < m.r + 1.5);
      const seg = (x0, y0, r0, x1, y1, r1) => {
        const dx = x1 - x0, dy = y1 - y0, L = Math.hypot(dx, dy); if (L < 1) return '';
        const n = Math.ceil(L * 4), at = t => [x0 + dx * t, y0 + dy * t];
        let a = 0, b = n; while (a <= n && covered(...at(a / n))) a++; while (b >= 0 && covered(...at(b / n))) b--;
        if (b - a < 4) return '';
        const [ax, ay] = at(a / n), [bx, by] = at(b / n);
        return `M${ax.toFixed(1)} ${ay.toFixed(1)}L${bx.toFixed(1)} ${by.toFixed(1)}`;
      };
      let d = '';
      for (let i = 0; i + 1 < fix.length; i++) {
        const f = fix[i], g = fix[i + 1];
        if (inside(f) && inside(g)) d += seg(X(f.sx), Y(f.sy), rad(f), X(g.sx), Y(g.sy), rad(g));
        else if (inside(f) !== inside(g)) { const a2 = inside(f) ? f : g, b2 = inside(f) ? g : f, e = edge(a2, b2); d += seg(X(a2.sx), Y(a2.sy), rad(a2), X(e.x), Y(e.y), 0) + `M${X(e.x).toFixed(1)} ${Y(e.y).toFixed(1)}` + (e.horiz ? 'm0 -3v6' : 'm-3 0h6'); }
      }
      s += `<path d="${d}" fill="none" stroke="var(--path)" stroke-width="1" stroke-linecap="round"/>`;
    }
    if (o.layers.points) {
      for (const f of fix) {
        const r = Math.sqrt(f.dur / 1000) * 13 * Math.max(0.55, k / 0.32), x = X(f.sx), y = Y(f.sy);
        if (!f.inWin) { s += `<circle cx="${x}" cy="${y}" r="${r}" fill="none" stroke="var(--stage-ink)" stroke-width="1.25" stroke-dasharray="2 2"/>`; continue; }
        if (o.role === 'query') s += `<circle cx="${x}" cy="${y}" r="${r}" fill="var(--query)" stroke="#fff" stroke-width="1.25"/>`;
        else if (o.role === 'match') { const q = r * 1.25; s += `<path d="M${x} ${y - q}L${x + q} ${y}L${x} ${y + q}L${x - q} ${y}Z" fill="var(--match)" stroke="#fff" stroke-width="1.25"/>`; }
        else if (o.role === 'control') s += `<circle cx="${x}" cy="${y}" r="${r}" fill="none" stroke="#E8A23C" stroke-width="2"/>`;
        else s += `<circle cx="${x}" cy="${y}" r="${r}" fill="rgba(28,30,33,.45)" stroke="#fff" stroke-width="1.5"/>`;
        if (o.selected === f.ord) s += `<circle cx="${x}" cy="${y}" r="${r + 2}" fill="none" stroke="#fff" stroke-width="4"/><circle cx="${x}" cy="${y}" r="${r + 2}" fill="none" stroke="#1C1E21" stroke-width="2"/>`;
      }
    }
    if (o.display === 'blank' && !o.frameOnly) {
      const cx = X(960), cy = Y(540);
      s += `<path d="M${cx - 5} ${cy}h10M${cx} ${cy - 5}v10" stroke="#1C1E21" stroke-width="3"/><path d="M${cx - 5} ${cy}h10M${cx} ${cy - 5}v10" stroke="#fff" stroke-width="1.25"/>`;
    }
    s += '</svg>';
    return { svg: s, max: G.max, fix };
  };
  ES.sharedLimit = (keys, sigma) => Math.max(...keys.map(k => ES.grid(ES.fixations(k), sigma).max));

  // map legend strip (8 swatches as drawn) + isoline sample
  ES.tog = (label, on) => `<button class="tog${on ? ' on' : ''}" aria-pressed="${on}"><span class="box">${on ? '<svg width="9" height="9" viewBox="0 0 10 10" fill="none" stroke="currentColor" stroke-width="2"><path d="m2 5 2 2 4-4.5"/></svg>' : ''}</span>${label}</button>`;
  ES.closeInspector = `<button class="ibtn" aria-label="Close the inspector (⌘I)">${'<svg width="16" height="16" viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1.5" aria-hidden="true"><rect x="2.5" y="3" width="11" height="10" rx="1"/><path d="M10 3v10"/></svg>'}</button>`;
  ES.addToFigure = `<button class="btn sm" aria-haspopup="menu"><svg width="16" height="16" viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1.5" aria-hidden="true"><path d="M8 3v10M3 8h10"/></svg>Add to figure<svg width="12" height="12" viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1.5" aria-hidden="true"><path d="m4 6 4 4 4-4"/></svg></button>`;
  ES.mapLegend = function (extra, shared = true) {
    let sw = '';
    for (let i = 0; i < 8; i++) {
      const t = i / 7, [r, g, b] = ES.ramp(t), a = Math.min(1, t / ES.FLOOR) * ES.MAP_OPACITY;
      sw += `<rect x="${i * 8}" y="0" width="8" height="8" fill="rgb(${r},${g},${b})" fill-opacity="${a.toFixed(3)}"/>`;
    }
    return `<span class="it row">0<svg width="64" height="8" style="background:#808080">${sw}</svg>max density${shared ? ' · shared' : ''} · photo in grey</span>` +
      `<span class="it row"><svg width="18" height="8"><path d="M1 4h16" stroke="rgba(255,255,255,.6)" stroke-width="3.5"/><path d="M1 4h16" stroke="#1C1E21" stroke-width="1.5"/></svg>50 · 90% of mass</span>` + (extra || '');
  };

  ES.fmt = (v, d = 2) => (v >= 0 ? '+' : '−') + Math.abs(v).toFixed(d);
  ES.f2 = v => v.toFixed(2);
  ES.part = id => F.participants.find(p => p.id === id);
  ES.mean = a => a.reduce((x, y) => x + y, 0) / a.length;
})();
