// MAP_RECIPE v2 (round 2: alpha floor, owner decision 2026-10-11) — paste these functions (unchanged) inside a board's `class Component extends DCLogic`
// script, ABOVE the class, and call massMap(...) from renderVals(). It mirrors the desktop exactly:
//  - studio/app/.../maps/MapPalette.scala `MapColours.along`: 4 evenly spaced ramp stops
//    (ramp-0..3 = #F9D9EA #E58FB9 #B83F86 #6B0F4F), channel-wise sRGB interpolation, opaque per cell,
//    one global opacity (MapOpacity.Default 0.6) applied to the whole raster group.
//  - studio/app/.../maps/Isolines.scala: marching squares over cell centres, linear crossing along
//    edges, saddles resolved by the 4-corner mean; levels = 50% and 90% of mass (UI-E).
//  - TrialScene.scala:379-383: isoline ink 2 px over a 4 px casing (stage halo), drawn as
//    straight Segments (Intaglio has no curves) => emit one <path> of M/L pairs, never curves.
// The grid is 64x48 on the image frame (DESIGN_SPEC §9). Draw cells as <rect>s in an <svg> group with
// opacity="0.6" (use shape-rendering="crispEdges").
// ALPHA FLOOR (owner decision, amends DESIGN_SPEC §6 "opaque per cell"): a cell's alpha rises linearly from
// 0 at zero mass to 1 at FLOOR (10%) of the colour-limit max; colour is the ramp colour as before. So the
// stimulus/stage shows through low-mass regions. Coverage is shown by the frame outline, not by ramp-0 paint;
// zero mass inside the frame is therefore drawn transparent, and missing data (NoCoverage) is never drawn
// as a colour either — a map with missing cells must say so in its caption (missing ≠ zero still holds:
// a missing MAP is the dashed "no value" state, never an empty frame). Desktop change: MapColours.argb
// gains alpha = min(1, t / 0.10) where t is the fraction along the limits (ticket to file on landing).
// Values are display mock-ups derived from the board's own fixation list: same fixations as drawn.
const RAMP = [[0xF9, 0xD9, 0xEA], [0xE5, 0x8F, 0xB9], [0xB8, 0x3F, 0x86], [0x6B, 0x0F, 0x4F]];
function rampColour(t) {
  t = Math.max(0, Math.min(1, t));
  const seg = RAMP.length - 1, at = t * seg, k = Math.min(seg - 1, Math.floor(at)), f = at - k;
  const a = RAMP[k], b = RAMP[k + 1];
  const c = [0, 1, 2].map(i => Math.round(a[i] + (b[i] - a[i]) * f));
  return 'rgb(' + c.join(',') + ')';
}
// fix: [{x, y, dur}] in IMAGE pixels (1024x768 frame); sigmaPx = σ in image px (35 px/°: σ 2° = 70 px).
// w, h: drawn size of the frame in the board (px). Returns {cells, iso50, iso90, max} where cells are
// {x, y, w, h, fill} in drawn px and iso50/iso90 are SVG path strings of straight segments.
const FLOOR = 0.10;
// limitMax (optional): shared colour-limit maximum (Shared limits across panels); default = this map's max.
function massMap(fix, sigmaPx, w, h, gw, gh, limitMax) {
  gw = gw || 64; gh = gh || 48;
  const cw = 1024 / gw, ch = 768 / gh, g = [];
  let total = 0, max = 0;
  for (let j = 0; j < gh; j++) {
    const row = [];
    for (let i = 0; i < gw; i++) {
      const cx = (i + 0.5) * cw, cy = (j + 0.5) * ch;
      let v = 0;
      for (const f of fix) { const dx = cx - f.x, dy = cy - f.y; v += f.dur * Math.exp(-(dx * dx + dy * dy) / (2 * sigmaPx * sigmaPx)); }
      row.push(v); total += v; if (v > max) max = v;
    }
    g.push(row);
  }
  const sx = w / gw, sy = h / gh, cells = [], lim = limitMax || max;
  for (let j = 0; j < gh; j++) for (let i = 0; i < gw; i++) {
    const t = lim ? g[j][i] / lim : 0, op = Math.min(1, t / FLOOR);
    if (op < 0.01) continue; // transparent: stage/stimulus shows through
    cells.push({ x: +(i * sx).toFixed(2), y: +(j * sy).toFixed(2), w: +(sx + 0.05).toFixed(2), h: +(sy + 0.05).toFixed(2), fill: rampColour(t), op: +op.toFixed(3) });
  }
  // level enclosing `share` of the mass: descending cell values, cumulative sum
  const sorted = g.flat().sort((a, b) => b - a);
  const levelFor = share => { let acc = 0; for (const v of sorted) { acc += v; if (acc >= share * total) return v; } return 0; };
  const contour = level => {
    const P = (i, j) => [((i + 0.5) * sx), ((j + 0.5) * sy)];
    const lerp = (p, q, v0, v1) => { const t = v1 === v0 ? 0.5 : (level - v0) / (v1 - v0); return [p[0] + (q[0] - p[0]) * t, p[1] + (q[1] - p[1]) * t]; };
    let d = '';
    const seg = (p, q) => { d += 'M' + p[0].toFixed(1) + ' ' + p[1].toFixed(1) + 'L' + q[0].toFixed(1) + ' ' + q[1].toFixed(1); };
    for (let j = 0; j < gh - 1; j++) for (let i = 0; i < gw - 1; i++) {
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
  };
  return { cells, iso50: contour(levelFor(0.5)), iso90: contour(levelFor(0.9)), max };
}
// Markup (inside the frame's <svg> sized w x h, after the stimulus/stage and BEFORE the marks):
//   <g opacity="0.6" shape-rendering="crispEdges"><sc-for list="{{map.cells}}" as="c" hint-placeholder-count="1">
//     <rect x="{{c.x}}" y="{{c.y}}" width="{{c.w}}" height="{{c.h}}" fill="{{c.fill}}" fill-opacity="{{c.op}}"></rect></sc-for></g>
//   <path d="{{map.iso90}}" fill="none" stroke="var(--isoline-case)" stroke-width="4" stroke-linecap="round"></path>
//   <path d="{{map.iso90}}" fill="none" stroke="var(--isoline-ink)" stroke-width="2" stroke-linecap="round"></path>
//   (same pair for iso50)
// Define --isoline-case / --isoline-ink in the token block if the board lacks them:
//   light & dark: --isoline-case: rgba(255,255,255,.9); --isoline-ink: rgba(28,30,33,.9)  (DESIGN_SPEC §6).
// Add a compact legend under/beside every map: a 4-stop ramp strip built from 8 <rect>s via rampColour,
// labelled "fixation duration mass per cell · 0 → max", and "isolines: 50% · 90% of mass".

// LEGEND (one component on every board, drawn as the map is drawn): mapLegend() returns 8 swatches whose
// fill/opacity reproduce the map at the stage: render each as <rect fill="{{s.fill}}" fill-opacity="{{s.op}}">
// inside a <g opacity="0.6"> on a stage-coloured strip (var(--stage) background, 1px hairline), then
// text in this order and wording, 11 px:  "0" [strip] "max · fixation duration mass per cell" · [isoline
// sample: 4px casing + 2px ink, 16px] "50% · 90% of mass" (+ " · shared limits" when limits are shared).
function mapLegend() {
  const out = [];
  for (let k = 0; k < 8; k++) { const t = k / 7; out.push({ x: k * 10, fill: rampColour(t), op: +Math.min(1, t / FLOOR).toFixed(3) }); }
  return out;
}
// FIXATION CROSS (blank + fixation cross displays): draw it ABOVE the map raster and below the marks, as two
// straight Segments (desktop TrialScene uses the stage halo token for the cross): 2 px var(--halo) strokes
// cased by a 4 px var(--isoline-ink) underlay so it reads on any stage and over any map colour.
// MARK CASING: marks are cased with the STAGE halo token var(--halo) (TrialScene), never a themed token.
