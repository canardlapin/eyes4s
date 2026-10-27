#!/usr/bin/env python3
"""Regenerate the macOS icon with the light-theme colours from Studio's token source."""
from pathlib import Path
import re
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[2]
tokens = (ROOT / "studio/app/src/main/scala/eyes4s/studio/app/tokens/Tokens.scala").read_text()
colours = [re.search(r"case ThemedToken\." + token + r"\s+=> Colour.srgb\(0x([0-9a-f]+)\)", tokens)[1]
           for token in ("Accent", "OnAccent", "AccentSoft")]
with tempfile.TemporaryDirectory(prefix="eyes-icon-") as tmp:
    iconset = Path(tmp) / "EyesStudio.iconset"
    iconset.mkdir()
    subprocess.run(["/usr/bin/swift", "-module-cache-path", str(Path(tmp) / "cache"),
                    str(ROOT / "studio/packaging/RenderIcon.swift"), str(iconset), *colours], check=True)
    subprocess.run(["/usr/bin/iconutil", "-c", "icns", str(iconset), "-o",
                    str(ROOT / "studio/packaging/EyesStudio.icns")], check=True)
