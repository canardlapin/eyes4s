#!/usr/bin/env python3
"""Build an internal macOS candidate and verify its bundled runtime, without opening a window."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import platform
import plistlib
import subprocess
import tempfile
import zipfile

ROOT = Path(__file__).resolve().parents[2]
NAME = "Eyes Studio"
IDENTIFIER = "io.github.canardlapin.eyes-studio"


def run(args, **kwargs):
    return subprocess.run(args, check=True, text=True, **kwargs)


def git(*args):
    return run(["git", *args], cwd=ROOT, capture_output=True).stdout.strip()


def sha(path):
    with path.open("rb") as stream:
        digest = hashlib.sha256()
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
        return digest.hexdigest()


def source_state():
    # Include untracked source files; ignore build outputs and private fixtures.
    paths = subprocess.check_output(["git", "ls-files", "-z", "--cached", "--others",
                                     "--exclude-standard"], cwd=ROOT).split(b"\0")
    entries = [(os.fsdecode(p), sha(ROOT / os.fsdecode(p))) for p in sorted(set(paths))
               if p and (ROOT / os.fsdecode(p)).is_file()]
    return {"commit": git("rev-parse", "HEAD"), "tree": git("rev-parse", "HEAD^{tree}"),
            "dirty": bool(git("status", "--porcelain")),
            "files_sha256": hashlib.sha256(json.dumps(entries).encode()).hexdigest()}


def inspect_input(directory):
    jars = sorted(directory.glob("*.jar"))
    if not jars or not (directory / "studio-launcher.jar").is_file():
        raise RuntimeError("Missing staged runtime classpath")
    forbidden = []
    for jar in jars:
        with zipfile.ZipFile(jar) as archive:
            for name in archive.namelist():
                if (name.startswith(("org/testfx/", "org/junit/", "munit/")) or
                        name.startswith("com/sun/glass/ui/monocle/")):
                    forbidden.append(f"{jar.name}:{name}")
    if forbidden:
        raise RuntimeError(f"Test-only classes leaked into package: {forbidden[:5]}")
    return [{"name": p.name, "bytes": p.stat().st_size, "sha256": sha(p)} for p in jars]


def configure_document_type(app):
    path = app / "Contents/Info.plist"
    with path.open("rb") as stream:
        info = plistlib.load(stream)
    # .eyes is a directory package, not a flat file. Export a package UTI so
    # Finder opens the document with Studio rather than browsing its internals.
    uti = IDENTIFIER + ".project"
    info["CFBundleDocumentTypes"] = [{
        "CFBundleTypeName": "Eyes Studio project", "CFBundleTypeRole": "Editor",
        "LSHandlerRank": "Owner", "LSTypeIsPackage": True,
        "LSItemContentTypes": [uti], "CFBundleTypeExtensions": ["eyes"]}]
    info["UTExportedTypeDeclarations"] = [{
        "UTTypeIdentifier": uti, "UTTypeDescription": "Eyes Studio project",
        "UTTypeConformsTo": ["com.apple.package", "public.content"],
        "UTTypeTagSpecification": {"public.filename-extension": ["eyes"]}}]
    with path.open("wb") as stream:
        plistlib.dump(info, stream, sort_keys=False)


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("--output", type=Path, required=True, help="New output directory")
    p.add_argument("--allow-dirty", action="store_true", help="Iteration only; recorded in receipt")
    args = p.parse_args()
    if platform.system() != "Darwin":
        p.error("macOS package must be built on macOS")
    java = Path(os.environ.get("JAVA_HOME", ""))
    if not java.is_absolute() or not (java / "bin/jpackage").is_file():
        p.error("Set JAVA_HOME to the supported JDK 25 installation")
    version = run([str(java / "bin/java"), "-version"], capture_output=True).stderr
    if 'version "25.' not in version and 'version "25"' not in version:
        p.error("Use JDK 25 for this candidate")
    before = source_state()
    if before["dirty"] and not args.allow_dirty:
        p.error("Commit the candidate first, or use --allow-dirty for iteration")
    output = args.output.resolve()
    output.mkdir(parents=True, exist_ok=False)
    with (output / "stage.log").open("w") as log:
        run(["sbt", "-batch", "-J-Xmx4g", "studioDesktop/studioPackageInput"],
            cwd=ROOT, stdout=log, stderr=subprocess.STDOUT)
    if source_state() != before:
        raise RuntimeError("Sources changed while staging; candidate refused")
    staged = ROOT / "target/studio-package/input"
    inputs = inspect_input(staged)
    command = [str(java / "bin/jpackage"), "--type", "app-image", "--name", NAME,
               "--dest", str(output), "--input", str(staged),
               "--main-jar", "studio-launcher.jar", "--main-class",
               "eyes4s.studio.desktop.StudioMain", "--app-version", "1.0.0",
               "--mac-package-identifier", IDENTIFIER, "--mac-package-name", NAME,
               "--mac-app-category", "public.app-category.education",
               "--vendor", "canardlapin", "--copyright", "Copyright 2026 canardlapin",
               "--description", "Eyes Studio internal test candidate",
               "--add-modules", "java.se,jdk.unsupported,jdk.crypto.ec,jdk.management,jdk.jfr,jdk.localedata,jdk.charsets,jdk.zipfs", "--java-options", "--enable-native-access=ALL-UNNAMED"]
    icon = ROOT / "studio/packaging/EyesStudio.icns"
    if icon.is_file():
        command += ["--icon", str(icon)]
    with (output / "jpackage.log").open("w") as log:
        run(command, cwd=ROOT, stdout=log, stderr=subprocess.STDOUT)
    app = output / (NAME + ".app")
    configure_document_type(app)
    # Changing Info.plist invalidates jpackage's ad-hoc seal. Seal the final
    # bundle locally; this is NOT a Developer ID signature or notarization.
    run(["/usr/bin/codesign", "--force", "--deep", "--sign", "-", str(app)], capture_output=True)
    run(["/usr/bin/codesign", "--verify", "--deep", "--strict", str(app)], capture_output=True)
    check = output / "runtime-check"
    with tempfile.TemporaryDirectory(prefix="eyes-studio-cwd-") as cwd:
        env = {k: v for k, v in os.environ.items()
               if k not in ("JAVA_HOME", "JDK_HOME", "CLASSPATH", "JAVA_TOOL_OPTIONS", "JDK_JAVA_OPTIONS", "_JAVA_OPTIONS")}
        env["PATH"] = "/usr/bin:/bin"
        with (output / "runtime-check.log").open("w") as log:
            run([str(app / "Contents/MacOS" / NAME), "--package-check", str(check)],
                cwd=cwd, env=env, stdout=log, stderr=subprocess.STDOUT, timeout=180)
    receipt = json.loads((check / "package-check.json").read_text())
    runtime = (app / "Contents/runtime").resolve()
    if not Path(receipt["java_home"]).resolve().is_relative_to(runtime):
        raise RuntimeError("Launcher used an external Java installation")
    if receipt["commit"] != before["commit"] or receipt["status"] != "passed":
        raise RuntimeError("Packaged source identity or self-check does not match")
    archive = output / f"Eyes-Studio-{platform.machine()}-{before['commit'][:12]}.zip"
    run(["/usr/bin/ditto", "-c", "-k", "--sequesterRsrc", "--keepParent", str(app), str(archive)])
    report = {"schema": 1, "status": "passed", "source": before,
              "architecture": platform.machine(), "jdk": version.strip(),
              "signing": "ad-hoc only; no Developer ID or notarization",
              "input_jars": inputs, "runtime_check": receipt,
              "archive": {"file": archive.name, "bytes": archive.stat().st_size, "sha256": sha(archive)},
              "unqualified": ["native UI from installed app", "Finder file association dispatch",
                              "clean macOS 14 with no JDK", "Gatekeeper distribution acceptance",
                              "physical Retina review"]}
    (output / "package.json").write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps({"status": "passed", "archive": str(archive), "receipt": str(output / "package.json")}))


if __name__ == "__main__":
    main()
