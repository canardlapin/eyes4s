#!/usr/bin/env python3
"""Fingerprint every source/test/resource/build input to the native story court."""
from pathlib import Path
import hashlib
import sys

MODULES = ('kernel', 'core', 'detect', 'surface', 'aoi', 'compare', 'design', 'plan',
           'results', 'codec', 'laws', 'fs2', 'io', 'studio/core', 'studio/app',
           'studio/viz', 'studio/desktop')
BASELINES = 'studio/desktop/src/test/resources/eyes4s/studio/desktop/visual-parity/'

def inputs(root):
    files = set(root.glob('*.sbt'))
    for name in MODULES:
        for axis in ('', '.jvm', '.js', 'jvm', 'js'):
            source = root / name / axis / 'src'
            if source.exists():
                files.update(p for p in source.rglob('*') if p.is_file()
                             and not p.relative_to(root).as_posix().startswith(BASELINES))
    project = root / 'project'
    if project.exists():
        files.update(p for p in project.iterdir()
                     if p.is_file() and p.suffix in ('.scala', '.sbt', '.properties'))
    fixture = root / 'fixtures/studio-golden'
    if fixture.exists():
        files.update(p for p in fixture.rglob('*') if p.is_file())
    for name in ('fixture.json', 'FIXTURE.md'):
        p = root / 'docs/studio/fixture' / name
        if p.is_file():
            files.add(p)
    tools = root / 'tools/studio-visual-parity'
    if tools.exists():
        files.update(p for p in tools.iterdir() if p.is_file() and p.suffix in ('.py', '.json'))
    return sorted(files, key=lambda p: p.relative_to(root).as_posix())

def fingerprint(root):
    root = root.resolve()
    value = hashlib.sha256()
    for path in inputs(root):
        value.update(path.relative_to(root).as_posix().encode() + b'\0')
        value.update(hashlib.sha256(path.read_bytes()).digest())
    return value.hexdigest()

if __name__ == '__main__':
    print(fingerprint(Path(sys.argv[1]) if len(sys.argv) > 1 else Path(__file__).resolve().parents[2]))
