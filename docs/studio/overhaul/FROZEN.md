# Frozen porting golden: v4h (2026-10-29), amended v4i (2026-10-30)

Build: out/v4h (src at freeze; gate logs/v4h-gate.log EXIT 0). Golden renders + region maps: parity/golden-v4h (Chromium, 1440x900 @1x).
Parity tool: parity/parity.js (thresholds and negative controls in parity/README.md). Verification of the v4e critics' freeze items: audit/verify-v4g.md (19/21; the 2 failures fixed in v4h and guarded by the gate's order-line check, logs/v4g-negative-control-orderlines.log).

```
10230c008d3d9886beeeed6402a5f01ef7722f8f8a98540ac8e580e2e15f0bae  analysis.html
1356edd746dd9f3d0f55acf64be681feea92ef5bc91e020340611e08a1df35b0  compare-query-dark.html
1356edd746dd9f3d0f55acf64be681feea92ef5bc91e020340611e08a1df35b0  compare-query.html
fc3b6fc59fcff2d7dabe797861c5c8936c84d79d4fadf2d06da3ca66dc589782  data-trials.html
ec436060d6f2c516ff7fc2c07c8fa9114a7984f246ecd262410f3079a25319b7  data.html
c349e911c6a3a01b45e7584ec4142e37ff5afcb231733e9c43cb8f324bb6cfc1  explore.html
cd885a51c0cdf34e618da48e6e2bfe9c310a283a3fb4e62e109d17cdaf271651  figures.html
5669e00c55ae35766aab4302cbff68883e6b04074db19139b9a5a32af71f5694  methods.html
7996386b251e19c7403309a91307832bbe12769d6967a547dfbe55ce5e89eeba  results.html
8e3ec0ba2cac393dc9bdd4269c01b85ee01fea3ff2bdd432d49acca84b88b6b1  state-menu.html
14046448d138ea8ce943f8e8f04948d0e3db448488a0cfc957634706c940853b  es.css
dd287b1eac8c0c60748d3b59bbf4ef6b5725e8d6a2c24139fbcf19f7517ffa34  es.js
b49cef8c6a2b47c1c4c6577d1646c49aa82df4842302cd2e10dd61b0844ad85a  data.js

ced601a88812448dd6014a257050484916211c83511dcc3d2f3e9e3d66d42ad6  analysis.png
dc4cb4d663ea756982339414656068f0e556bc3d999f71e7732326b1168f0a6d  analysis.regions.json
1e5a02dd161c96ab87d038f2c8c0f4214bd40687e19784596e8cfb2639e2708c  compare-query-dark.png
667ebe5baf417d78b2739b2f92141c039371c32127c54a8cfc732816eb9fbef3  compare-query-dark.regions.json
320b6c9f2e1d8edbf629fd8f47de4932104e2919980dd3a0f3ed2647bdf5ffb9  compare-query.png
667ebe5baf417d78b2739b2f92141c039371c32127c54a8cfc732816eb9fbef3  compare-query.regions.json
dd2295e160dff86f235833d60abe7327a3058559e4001f23afdcc279f4d7e838  data-trials.png
736948530608163f4b79d5862f0f84fcfb617260f9b16a2681eae1cda2cd2fc3  data-trials.regions.json
d051bd32ef934cfc3eb031f75cf7b7dbc5189e381d9e434e438618f56bb935e8  data.png
90d93160b5a05afa77a7042367eef719802c21627886b1a138a891320334ce5c  data.regions.json
a7678fb8423958e87de15b77a1f097c6f747caf35a6eaf6e63c1992ba1f900d4  explore.png
e525bb459d37106c95df95742d987f5c77af73365a5c98ae5fdbbef5df3ee7d5  explore.regions.json
e551aa43e1a2e10e6a0cf0829af391a8d949a332c73b963a0f0952eb0680656c  figures.png
ae4d2a0ee2df9f7bfb2e08a250cb457f244e6d3735f370bef2eda6328fb616d3  figures.regions.json
757948505f04bae1da5a12adc646c2361228a91781abb8791c67729c7f193264  methods.png
ccd6543f4b777fa9776881b98d47684b4973e3ed9338c5bd21d9b4e1c6a334e6  methods.regions.json
89a610ad4c0161a25ab5e14e326555a05498bd830174cf76966d769a2f73bf48  results.png
c44d5cb7874349a9d39d6cc286ddb945a54e0c715591080f430e72af4873f2dd  results.regions.json
bc378d6e76764a3a10fb052b913cf3cdf3aaeedc5183d9f3e80493a2b2b9f1d7  state-menu.png
03ce4e91ed128d8778fd690aef4c66b2a2652f075b8603e2740e4d2f2ca314f2  state-menu.regions.json
```

Amendment v4i, 2026-10-30 (Port 1, accessibility laws; owner: "be flexible, push forward with the redesign and adapt laws as necessary"):
- Isoline casing on maps (es.js) is white at 60%, not 35%; the legend sample matches it (was 90%). Over --ramp-1 the
  35% casing and the ink both fell below 3:1 (2.15, 2.34); at 60% the casing reaches 3.49. Re-rendered: compare-query,
  compare-query-dark, data-trials, explore (pixels inside the map frames only; region maps unchanged).
- Laws adapted to the golden rather than the golden to the laws: adjacent density levels are kept apart by 10 L*
  (the v4 ramp steps 12.76, 11.02, 11.68) instead of a 1.5 contrast ratio, which compressed the even dark end to 1.46;
  query and matched stay 12 L* apart in both themes (light 15.0, dark 13.3) instead of 20, since the marks also
  differ in shape and the pills name the role. Density casing tables re-derived from the new ramp.
