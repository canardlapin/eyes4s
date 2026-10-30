#!/usr/bin/env bash
# Copies the fonts and stimulus images the mockups reference (not committed twice): run once before opening the pages.
set -euo pipefail
here=$(cd -- "$(dirname -- "$0")" && pwd); root=$(cd -- "$here/../../../.." && pwd)
mkdir -p "$here/fonts" "$here/img"
cp "$root"/studio/desktop/src/main/resources/eyes4s/studio/desktop/fonts/*.ttf "$here/fonts/"
for i in $(ls "$root/fixtures/studio-golden/stimuli"); do cp "$root/fixtures/studio-golden/stimuli/$i" "$here/img/"; done
