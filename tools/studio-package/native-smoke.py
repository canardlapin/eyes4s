#!/usr/bin/env python3
"""Observe the actual packaged app receiving a macOS Open File event. Opens a native window."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import time
import zipfile

ROOT = Path(__file__).resolve().parents[2]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--package', type=Path, required=True, help='Output directory of build.py')
    parser.add_argument('--allow-visible', action='store_true', help='Run only on an available test desktop')
    args = parser.parse_args()
    if not args.allow_visible:
        parser.error('This test opens a real OS window; explicitly pass --allow-visible on a test desktop')
    package = args.package.resolve()
    app = package / 'Eyes Studio.app'
    output = package / 'native-smoke'
    output.mkdir(exist_ok=False)
    expected = 'Package smoke α'
    (output / 'expected-title.txt').write_text(expected + '\n')
    project = output / (expected + '.eyes')
    shutil.copytree(package / 'runtime-check/memory-study.eyes', project)
    java = Path(os.environ['JAVA_HOME'])
    classes = output / 'probe-classes'
    classes.mkdir()
    # Compile against the *packaged* runtime jars, not checkout class products.
    jars = sorted((app / 'Contents/app').glob('*.jar'))
    subprocess.run([str(java / 'bin/javac'), '-cp', os.pathsep.join(map(str, jars)), '-d', str(classes),
                    str(ROOT / 'tools/studio-package/EyesStudioPackageProbe.java')], check=True)
    agent = output / 'observer.jar'
    with zipfile.ZipFile(agent, 'w') as archive:
        archive.writestr('META-INF/MANIFEST.MF', 'Manifest-Version: 1.0\nPremain-Class: EyesStudioPackageProbe\n\n')
        for path in classes.rglob('*.class'):
            archive.write(path, path.relative_to(classes).as_posix())
    # open's --env applies to this new instance only; no machine-wide launchctl changes.
    # The agent observes the scene and requests ordinary window close; it cannot import
    # or seed a project. The separate `open -a` must deliver the actual OS document event.
    command = ['/usr/bin/open', '-n', '-W', '-a', str(app),
               '--env', f'JAVA_TOOL_OPTIONS="-javaagent:{agent}={output}"',
               '--env', 'JAVA_HOME=', '--env', 'CLASSPATH=', '--env', 'JDK_JAVA_OPTIONS=',
               '--env', '_JAVA_OPTIONS=', '--env', 'PATH=/usr/bin:/bin',
               '--stdout', str(output / 'stdout.log'), '--stderr', str(output / 'stderr.log')]
    started = time.monotonic()
    process = subprocess.Popen(command)
    try:
        for name in ['ready.json', 'native-smoke.json']:
            deadline = time.monotonic() + 120
            while not (output / name).exists():
                error = output / 'native-smoke-error.txt'
                if error.exists():
                    raise RuntimeError(error.read_text())
                if process.poll() is not None:
                    raise RuntimeError(f'App exited before {name}; inspect native logs')
                if time.monotonic() > deadline:
                    raise RuntimeError(f'Timed out waiting for {name}; inspect native logs')
                time.sleep(0.2)
            if name == 'ready.json':
                subprocess.run(['/usr/bin/open', '-a', str(app), str(project)], check=True)
        if process.wait(timeout=30) != 0:
            raise RuntimeError('Native application did not exit normally')
        report = json.loads((output / 'native-smoke.json').read_text())
        if not Path(report['java_home']).resolve().is_relative_to((app / 'Contents/runtime').resolve()):
            raise RuntimeError('Native app used an external JVM')
        report.update(schema=1, elapsed_seconds=time.monotonic()-started,
                      mechanism='Packaged native launcher; separate LaunchServices Open File; observer only',
                      scope='Native Mac launch/document-open/render/ordinary-close. No clean-macOS14, VoiceOver or physical Retina claim.',
                      screenshot_sha256=hashlib.sha256((output/'native-scene.png').read_bytes()).hexdigest())
        (output / 'native-smoke.json').write_text(json.dumps(report, indent=2)+'\n')
        print(json.dumps(report))
    finally:
        # open is our wait process; a test failure leaves the app/evidence for diagnosis.
        # On ephemeral CI the job owns the desktop. Never kill unrelated Java apps.
        if process.poll() is None:
            process.terminate()


if __name__ == '__main__':
    main()
