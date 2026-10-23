"""Evidence refusal tests; no JavaFX run or checked-in captures are rewritten."""
import copy
import hashlib
import importlib.util
import json
from pathlib import Path
import tempfile
import unittest

spec = importlib.util.spec_from_file_location('board_check', Path(__file__).with_name('check.py'))
check = importlib.util.module_from_spec(spec)
spec.loader.exec_module(check)


class EvidenceRefusalTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.previous_root = check.ROOT
        check.ROOT = Path(self.temp.name)
        self.addCleanup(setattr, check, 'ROOT', self.previous_root)
        self.reference = self.write('docs/studio/design/reference/Main-1x.png', b'reference')
        self.capture = self.write('native.png', b'native')
        self.metrics = self.write('metrics.json', json.dumps({
            'board': 'Main', 'width': 1440, 'height': 900,
            'heights': {'appBar': 44, 'contextStrip': 32, 'statusBar': 24},
            'tabHeaders': [28, 28]
        }).encode())
        self.report = self.write('tests.xml', b'<testsuite><testcase name="native"/></testsuite>')
        self.review = {
            'schema': 1, 'board': 'Main', 'reviewed': True, 'findings': 'Visual comparison',
            'inputs': [self.reference], 'captures': [self.capture], 'metrics': self.metrics,
            'checks': [{'id': k, 'disposition': 'match', 'evidence': 'native bounds'}
                       for k in sorted(check.CHECKS['Main'])],
            'tests': [{'report': self.report['path'], 'case': 'native'}]
        }

    def write(self, name, content):
        p = check.ROOT / name
        p.parent.mkdir(parents=True, exist_ok=True)
        p.write_bytes(content)
        return {'path': name, 'sha256': hashlib.sha256(content).hexdigest()}

    def test_complete_receipt_passes(self):
        self.assertEqual(check.verify(self.review)['checks'], 7)

    def test_changed_capture_is_refused(self):
        (check.ROOT / self.capture['path']).write_bytes(b'changed after review')
        with self.assertRaisesRegex(ValueError, 'changed review input'):
            check.verify(self.review)

    def test_missing_check_is_refused(self):
        self.review['checks'].pop()
        with self.assertRaisesRegex(ValueError, 'Incomplete checklist'):
            check.verify(self.review)

    def test_skipped_failed_and_duplicate_named_tests_are_refused(self):
        for nodes in ['<testcase name="native"><skipped/></testcase>',
                      '<testcase name="native"><failure/></testcase>',
                      '<testcase name="native"/><testcase name="native"/>']:
            with self.subTest(nodes=nodes):
                self.write('tests.xml', ('<testsuite>' + nodes + '</testsuite>').encode())
                with self.assertRaisesRegex(ValueError, 'unique successful case'):
                    check.verify(self.review)

    def test_wrong_shell_height_is_refused_even_when_rehashed(self):
        value = json.loads((check.ROOT / self.metrics['path']).read_text())
        value['heights']['contextStrip'] = 35
        self.review['metrics'] = self.write('metrics.json', json.dumps(value).encode())
        with self.assertRaisesRegex(ValueError, 'contextStrip height'):
            check.verify(self.review)

    def test_unreviewed_and_unjustified_deviation_are_refused(self):
        unreviewed = copy.deepcopy(self.review)
        unreviewed['reviewed'] = False
        with self.assertRaisesRegex(ValueError, 'visual review'):
            check.verify(unreviewed)
        self.review['checks'][0]['disposition'] = 'accepted deviation'
        with self.assertRaisesRegex(ValueError, 'Deviation needs'):
            check.verify(self.review)


if __name__ == '__main__':
    unittest.main()
