#!/usr/bin/env python3
import copy
import importlib.util
import json
from pathlib import Path
import struct
import tempfile
import unittest
import xml.etree.ElementTree as ET

spec = importlib.util.spec_from_file_location('visual_check', Path(__file__).with_name('check.py'))
check = importlib.util.module_from_spec(spec)
spec.loader.exec_module(check)

class CoverageRefusals(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.manifest = json.loads(Path(__file__).with_name('manifest.json').read_text())
        source = self.root / check.SOURCE
        source.parent.mkdir(parents=True)
        source.write_text('fixed candidate source')
        suite = ET.Element('testsuite')
        for state in self.manifest['states']:
            ET.SubElement(suite, 'testcase', name=state)
            t, p, theme = state.split('-')
            value = dict(schema=1, state=state, moment=t, perspective=p, theme=theme,
                         mode='linux-monocle-goldens', nativeScaleX=1, nativeScaleY=1,
                         logicalWidth=1440, logicalHeight=900,
                         maximumChannelDelta=1, maximumChangedFraction=.001,
                         sourceSha256=check.sha(source), candidateFingerprint=check.candidate.fingerprint(self.root),
                         osName='Linux', glassPlatform='Monocle', prismOrder='sw', problems=[], captures=[])
            for scale in [1, 2]:
                name = f'{theme}-{scale}x.png'
                actual = self.root / 'target/studio-snapshots/VisualParitySuite' / state / name
                baseline = self.root / 'studio/desktop/src/test/resources/eyes4s/studio/desktop/visual-parity' / state / name
                data = b'\x89PNG\r\n\x1a\n' + struct.pack('>I', 13) + b'IHDR' + struct.pack('>II', 1440*scale, 900*scale) + b'\x08\x06\0\0\0'
                for path in [actual, baseline]:
                    path.parent.mkdir(parents=True, exist_ok=True)
                    path.write_bytes(data)
                diff = dict(width=1440*scale, height=900*scale, changedPixels=0,
                            maximumChannelDelta=0, changedFraction=0., accepted=True)
                value['captures'].append(dict(actual=str(actual.relative_to(self.root)),
                    baseline=str(baseline.relative_to(self.root)), width=1440*scale, height=900*scale,
                    actualSha256=check.sha(actual), baselineSha256=check.sha(baseline),
                    repeat=diff, comparison=copy.deepcopy(diff)))
            (actual.parent / 'review.json').write_text(json.dumps(value))
        self.xml = self.root / check.REPORT
        self.xml.parent.mkdir(parents=True, exist_ok=True)
        ET.ElementTree(suite).write(self.xml)
        self.state = self.manifest['states'][0]
        self.receipt = self.root / 'target/studio-snapshots/VisualParitySuite' / self.state / 'review.json'

    def edit(self, f):
        value = json.loads(self.receipt.read_text())
        f(value)
        self.receipt.write_text(json.dumps(value))

    def test_complete_matrix(self):
        self.assertEqual(len(check.verify(self.root, self.manifest)['states']), 30)

    def test_changed_production_input_is_stale_even_with_unchanged_capture_suite(self):
        source = self.root / 'studio/app/src/main/scala/Changed.scala'
        source.parent.mkdir(parents=True)
        source.write_text('changed production input')
        with self.assertRaisesRegex(ValueError, 'changed source/resource/build'):
            check.verify(self.root, self.manifest)

    def test_missing_state(self):
        self.receipt.unlink()
        with self.assertRaisesRegex(ValueError, 'Missing or duplicate'):
            check.verify(self.root, self.manifest)

    def test_skipped_or_duplicate_case(self):
        tree = ET.parse(self.xml)
        ET.SubElement(tree.getroot()[0], 'skipped')
        tree.write(self.xml)
        with self.assertRaisesRegex(ValueError, 'executed passing'):
            check.verify(self.root, self.manifest)
        tree.getroot()[0].remove(tree.getroot()[0][0])
        ET.SubElement(tree.getroot(), 'testcase', name=self.state)
        tree.write(self.xml)
        with self.assertRaisesRegex(ValueError, 'executed passing'):
            check.verify(self.root, self.manifest)

    def test_changed_image_or_source(self):
        self.edit(lambda v: v['captures'][0].update(actualSha256='changed'))
        with self.assertRaisesRegex(ValueError, 'changed actual'):
            check.verify(self.root, self.manifest)
        self.edit(lambda v: v.update(sourceSha256='changed'))
        with self.assertRaisesRegex(ValueError, 'changed source'):
            check.verify(self.root, self.manifest)

    def test_replaced_baseline_and_bad_dimensions(self):
        value = json.loads(self.receipt.read_text())
        baseline = self.root / value['captures'][0]['baseline']
        baseline.write_bytes(baseline.read_bytes() + b'replaced')
        with self.assertRaisesRegex(ValueError, 'reviewed baseline'):
            check.verify(self.root, self.manifest)
        self.edit(lambda v: v['captures'][0].update(width=9))
        with self.assertRaisesRegex(ValueError, 'dimensions'):
            check.verify(self.root, self.manifest)

    def test_failed_repeat_or_widened_policy(self):
        self.edit(lambda v: v['captures'][0]['repeat'].update(accepted=False))
        with self.assertRaisesRegex(ValueError, 'failed repeat'):
            check.verify(self.root, self.manifest)
        self.edit(lambda v: v.update(maximumChannelDelta=10))
        with self.assertRaisesRegex(ValueError, 'changed pixel policy'):
            check.verify(self.root, self.manifest)

    def test_golden_geometry_cannot_be_weakened(self):
        self.edit(lambda v: v.update(logicalWidth=1024, logicalHeight=656))
        with self.assertRaisesRegex(ValueError, 'golden scene dimensions'):
            check.verify(self.root, self.manifest)

    def test_native_functional_geometry_is_recorded_and_checked(self):
        value = json.loads(self.receipt.read_text())
        value.update(mode='functional-only', logicalWidth=1024, logicalHeight=656,
                     osName='Mac OS X', glassPlatform='platform-default')
        for scale, capture in enumerate(value['captures'], 1):
            actual = self.root / capture['actual']
            data = actual.read_bytes()
            actual.write_bytes(data[:16] + struct.pack('>II', 1024*scale, 656*scale) + data[24:])
            capture.update(width=1024*scale, height=656*scale, actualSha256=check.sha(actual))
            capture['repeat'].update(width=1024*scale, height=656*scale)
        self.receipt.write_text(json.dumps(value))
        self.assertEqual(len(check.verify(self.root, self.manifest, True)['states']), 30)
        self.edit(lambda v: v.update(logicalHeight=655))
        with self.assertRaisesRegex(ValueError, 'wrong capture dimensions'):
            check.verify(self.root, self.manifest, True)

    def test_functional_only_is_explicit(self):
        self.edit(lambda v: v.update(mode='functional-only'))
        with self.assertRaisesRegex(ValueError, 'golden comparisons required'):
            check.verify(self.root, self.manifest)
        self.assertEqual(len(check.verify(self.root, self.manifest, True)['states']), 30)

if __name__ == '__main__':
    unittest.main()
