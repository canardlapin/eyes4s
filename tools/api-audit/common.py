"""Paths and source identity shared by the API audit runner and gate."""
import hashlib
from pathlib import Path
ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / 'target/api-audit'
MODULES = ('kernel','core','detect','surface','aoi','compare','design','plan','results','codec','laws','fs2','io')
PROJECTS = {m: ('fs2Module' if m == 'fs2' else m) for m in MODULES}

def fingerprint():
    files = {ROOT / 'project/ApiAudit.scala', ROOT / 'README.md', ROOT / 'docs/formats/eyelink-asc.md'}
    # sbt loads every root .sbt file, including worktree-local source/generator settings.
    files.update(ROOT.glob('*.sbt'))
    for module in MODULES:
        for base in ('src', '.jvm/src', '.js/src'):
            directory = ROOT / module / base
            if directory.exists(): files.update(p for p in directory.rglob('*') if p.is_file())
    for base in ('tools/api-audit', 'project'):
        files.update(p for p in (ROOT/base).rglob('*') if p.is_file() and 'target' not in p.parts and '__pycache__' not in p.parts and p.suffix in ('.scala','.sbt','.py','.properties'))
    # Managed test inputs also belong to this candidate.
    for pattern in ('tools/*.py', 'tools/study-cli/*.scala', 'tools/r-parity/fixtures/*.json'):
        files.update(ROOT.glob(pattern))
    digest = hashlib.sha256()
    for p in sorted(files):
        digest.update(str(p.relative_to(ROOT)).encode()+b'\0'+p.read_bytes()+b'\0')
    return digest.hexdigest()
