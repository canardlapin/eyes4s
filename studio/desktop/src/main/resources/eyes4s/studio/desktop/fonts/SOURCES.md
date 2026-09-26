# Bundled fonts: sources and checksums

Eyes Studio bundles seven static TrueType faces (ticket S1.2, DESIGN_SPEC §7).
Each file is copied unmodified from an official upstream GitHub release asset,
downloaded with `curl` on 2026-09-26. `FontLoadSuite` recomputes every SHA-256
below and fails if a file differs.

All three families are licensed under the SIL Open Font License, Version 1.1.
The licence texts ship next to the fonts:

| Family         | Licence file              | Copyright                                   |
|----------------|---------------------------|---------------------------------------------|
| IBM Plex Sans  | `OFL-IBM-Plex-Sans.txt`   | © 2017 IBM Corp., Reserved Font Name "Plex" |
| IBM Plex Mono  | `OFL-IBM-Plex-Mono.txt`   | © 2017 IBM Corp., Reserved Font Name "Plex" |
| Source Serif 4 | `OFL-Source-Serif-4.txt`  | © 2014–2023 Adobe, Reserved Font Name "Source" |

Each licence file is the `LICENSE.txt` (Plex) or `LICENSE.md` (Source Serif)
at the root of the same release archive.

## Release assets

| Archive | Release | URL | SHA-256 of the archive |
|---------|---------|-----|------------------------|
| `ibm-plex-sans.zip` | `@ibm/plex-sans@1.1.0` | https://github.com/IBM/plex/releases/download/%40ibm%2Fplex-sans%401.1.0/ibm-plex-sans.zip | `fb365d910566e6d199cc2c15579a7dd9a267128e18431a394ed81f1970c69200` |
| `ibm-plex-mono.zip` | `@ibm/plex-mono@2.5.0` | https://github.com/IBM/plex/releases/download/%40ibm%2Fplex-mono%402.5.0/ibm-plex-mono.zip | `6d23f01257663d8cc49a0d64c22ced630b79e0e2a0ac08a0da86e9a38bbc481c` |
| `source-serif-4.005_Desktop.zip` | `4.005R` | https://github.com/adobe-fonts/source-serif/releases/download/4.005R/source-serif-4.005_Desktop.zip | `549fdb8f9a682bd06944298621404969f6de77c2e422ff3b8244a1dcd6a0c425` |

## Faces

| File | Weight | Path in the archive | SHA-256 |
|------|--------|---------------------|---------|
| `IBMPlexSans-Regular.ttf` | 400 | `ibm-plex-sans/fonts/complete/ttf/IBMPlexSans-Regular.ttf` | `975dcda37d80f038dcd143c22e33ca2d97a0cc5a929aace1c749153b0fe1afa5` |
| `IBMPlexSans-Medium.ttf` | 500 | `ibm-plex-sans/fonts/complete/ttf/IBMPlexSans-Medium.ttf` | `331c8639d7598b2cde62a911a71db195e30cb655cd6bdf2e324a7e984955f907` |
| `IBMPlexSans-SemiBold.ttf` | 600 | `ibm-plex-sans/fonts/complete/ttf/IBMPlexSans-SemiBold.ttf` | `a20caf8286023a6a7a85e40b1d2a4ae9fc3e3b1f9eda8f4c542dd4986af67bb1` |
| `IBMPlexMono-Regular.ttf` | 400 | `ibm-plex-mono/fonts/complete/ttf/IBMPlexMono-Regular.ttf` | `7c6fbddca4b700be918f5f6183d9bd4464fa427fe435f0b480d77fe2bb8c5a43` |
| `IBMPlexMono-Medium.ttf` | 500 | `ibm-plex-mono/fonts/complete/ttf/IBMPlexMono-Medium.ttf` | `98fbd727aae340b236955879dabed4d991aac9e8e90b3b2a67ce4a59221cc97c` |
| `SourceSerif4-Regular.ttf` | 400 | `source-serif-4.005_Desktop/TTF/SourceSerif4-Regular.ttf` | `e5a4ee6a3d87bb9024796be390c6771e2a0eb1883dae25effaf57ca01668e24b` |
| `SourceSerif4-Semibold.ttf` | 600 | `source-serif-4.005_Desktop/TTF/SourceSerif4-Semibold.ttf` | `36db62940cb5728b12b1802476dc7fcf4c6c519a7bdd476ba23a4e555fc4655f` |

## Family names

JavaFX selects a weight by family name only: every CSS weight below bold
resolves to the regular face. Each static face therefore carries its own legacy
family (OpenType name ID 1), and `studio-type.css` names that family. The
names are `IBM Plex Sans`, `IBM Plex Sans Medm`, `IBM Plex Sans SmBld`,
`IBM Plex Mono`, `IBM Plex Mono Medm`, `Source Serif 4` and
`Source Serif 4 Semibold` (`eyes4s.studio.app.tokens.FontFace`).
