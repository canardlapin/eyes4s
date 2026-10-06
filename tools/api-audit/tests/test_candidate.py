"""Fast protocol tests; real Scala coverage/audit qualification is a separate gate."""
import json
import shutil
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
import candidate
import common
import inventory
import run


def row(name='NewError', **changes):
    return dict(dict(kind='class', id='eyes4s.plan.' + name, name=name,
                     category='abstraction-or-container', flags='Flags.Enum', ownerFlags='',
                     signature='', valueType='', source='plan/src/main/scala/NewError.scala',
                     deprecated=False, inherited=['java.lang.Object', 'eyes4s.plan.Shared'],
                     sourceType='() extends scala.reflect.Enum'), **changes)


class CandidateTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.root = Path(self.tmp.name)
        self.out = self.root / 'target/api-audit'
        self.out.mkdir(parents=True)
        self.base = self.root / 'tools/api-audit'
        self.base.mkdir(parents=True)
        self.source = 'source-A'
        self.patches = [patch.object(module, name, value)
                        for module in (candidate, run)
                        for name, value in [('ROOT', self.root), ('OUT', self.out),
                                            ('fingerprint', lambda: self.source)]]
        for item in self.patches:
            item.start()
            self.addCleanup(item.stop)
        self.raw()
        self.run_id = 'a' * 32

    def raw(self):
        for axis in ('jvm', 'js'):
            (self.out / f'inventory-{axis}.json').write_text(json.dumps({'entries': [row()]}))

    def prepare(self, run_id=None):
        return candidate.prepare_inventory(self.source, run_id)

    def committed(self):
        path = self.base / 'inventory.json'
        path.write_text(inventory.inventory_text(inventory.public_inventory([row()], [row()])))
        candidate.write_provenance(self.base / 'inventory-provenance.json', path, self.source, 'committed')
        return path

    def test_prepared_is_explicit_fresh_and_not_execution_qualification(self):
        path = self.prepare()
        self.assertEqual(candidate.select_inventory({}), path)
        self.assertFalse((self.out / 'receipt.json').exists())
        self.source = 'edited-test-or-build'
        with self.assertRaisesRegex(ValueError, 'stale.*source/test/build'):
            candidate.select_inventory({})

    def test_committed_fallback_needs_provenance_and_ignores_audit_leftovers(self):
        committed = self.committed()
        self.prepare(self.run_id)
        self.assertEqual(candidate.select_inventory({}), committed)
        (self.base / 'inventory-provenance.json').unlink()
        with self.assertRaisesRegex(ValueError, 'run.py --prepare'):
            candidate.select_inventory({})

    def test_audit_never_falls_back_to_prepared_or_committed(self):
        self.prepare()
        self.committed()
        with self.assertRaisesRegex(ValueError, 'unavailable'):
            candidate.select_inventory({candidate.RUN_ENV: self.run_id})
        path = self.prepare(self.run_id)
        self.assertEqual(candidate.select_inventory({candidate.RUN_ENV: self.run_id}), path)
        with self.assertRaises(ValueError):
            candidate.select_inventory({candidate.RUN_ENV: 'b' * 32})
        candidate.retire(self.run_id)
        with self.assertRaisesRegex(ValueError, 'inactive'):
            candidate.select_inventory({candidate.RUN_ENV: self.run_id})

    def test_wrong_worktree_or_run_or_hash_or_malformed_metadata_refused(self):
        for field, bad in [('root', '/other'), ('run_id', 'b' * 32),
                           ('inventory_sha256', '0' * 64), ('version', 999)]:
            with self.subTest(field=field):
                self.prepare(self.run_id)
                _, provenance = candidate.paths(self.run_id)
                value = json.loads(provenance.read_text())
                value[field] = bad
                provenance.write_text(json.dumps(value))
                with self.assertRaises(ValueError):
                    candidate.select_inventory({candidate.RUN_ENV: self.run_id})
        provenance.write_text('not JSON')
        with self.assertRaises(ValueError):
            candidate.select_inventory({candidate.RUN_ENV: self.run_id})

    def test_stale_local_does_not_hide_behind_fresh_committed(self):
        local = self.prepare()
        self.committed()
        local.write_text('{}')
        with self.assertRaisesRegex(ValueError, 'hash'):
            candidate.select_inventory({})
        local.unlink()
        with self.assertRaises(ValueError):
            candidate.select_inventory({})

    def test_missing_local_provenance_and_malformed_inventory_fail_closed(self):
        path = self.prepare()
        _, provenance = candidate.paths()
        provenance.unlink()
        with self.assertRaises(ValueError):
            candidate.select_inventory({})
        for content in ('[]', '{}', '{"class Bad": ["incomplete"]}'):
            path.write_text(content)
            candidate.write_provenance(provenance, path, self.source, 'prepared')
            with self.assertRaisesRegex(ValueError, 'malformed'):
                candidate.select_inventory({})

    def test_source_change_during_compiler_generation_refused(self):
        with self.assertRaisesRegex(ValueError, 'sources changed'):
            candidate.prepare_inventory('before-edit', self.run_id)
        self.assertFalse(candidate.paths(self.run_id)[0].exists())

    def test_root_sbt_add_change_remove_invalidates_local_and_committed(self):
        for relative in ('build.sbt', 'project/ApiAudit.scala', 'README.md',
                         'docs/formats/eyelink-asc.md'):
            path = self.root / relative
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text('fixture')
        extra = self.root / 'zz-local.sbt'
        with patch.object(common, 'ROOT', self.root), \
             patch.object(candidate, 'fingerprint', common.fingerprint):
            for action in ('add', 'change', 'remove'):
                with self.subTest(action=action):
                    identity = common.fingerprint()
                    local = candidate.prepare_inventory(identity)
                    committed = self.base / 'inventory.json'
                    committed.write_bytes(local.read_bytes())
                    candidate.write_provenance(self.base / 'inventory-provenance.json',
                                               committed, identity, 'committed')
                    self.assertEqual(candidate.select_inventory({}), local)
                    if action == 'remove':
                        extra.unlink()
                    else:
                        extra.write_text(action)
                    self.assertNotEqual(common.fingerprint(), identity)
                    with self.assertRaisesRegex(ValueError, 'stale'):
                        candidate.select_inventory({})
                    shutil.rmtree(local.parent)
                    with self.assertRaisesRegex(ValueError, 'stale'):
                        candidate.select_inventory({})

    def test_invalid_run_identity_cannot_select_an_arbitrary_path(self):
        for value in ('', '../prepared', '/tmp/inventory', 'x' * 32):
            with self.assertRaises(ValueError):
                candidate.select_inventory({candidate.RUN_ENV: value})

    def test_prepare_runner_only_compiles_and_preserves_existing_test_evidence(self):
        report = self.root / 'io/.jvm/target/test-reports/TEST-existing.xml'
        report.parent.mkdir(parents=True)
        report.write_text('existing evidence')
        calls = []
        with patch.object(run, 'sbt', side_effect=lambda commands, *a, **k: calls.extend(commands)):
            run.main(['--prepare'])
        self.assertEqual(candidate.select_inventory({}), candidate.paths()[0])
        self.assertEqual(report.read_text(), 'existing evidence')
        self.assertFalse(any(c == 'test' or '/test' in c or 'Execution' in c or 'auditAgent' in c for c in calls))
        self.assertFalse((self.out / 'receipt.json').exists())

    def test_record_orders_candidate_before_tests_and_propagates_run_identity(self):
        agent = self.base / 'target/jacoco-agent.txt'
        agent.parent.mkdir()
        agent.write_text('agent.jar')
        steps = []
        def sbt(commands, *args, **kwargs):
            for command in commands:
                if command.endswith('/test'):
                    selected = candidate.select_inventory(kwargs['environment'])
                    self.assertIn('class eyes4s.plan.NewError', json.loads(selected.read_text()))
                    steps.append('test')
                elif command == 'test':
                    steps.append('tool-tests')
                elif 'Inventory ' in command:
                    steps.append('inventory')
        def process(command, **kwargs):
            if command[1].endswith('check.py'):
                self.assertEqual(command[-1], '--record')
                candidate.select_inventory(kwargs['env'])
                steps.append('record')
        with patch.object(run, 'sbt', side_effect=sbt), patch.object(run, 'MODULES', ('io',)), \
             patch.object(run.subprocess, 'run', side_effect=process), \
             patch.object(run.subprocess, 'check_output', return_value='sha'):
            run.main(['--record'])
        self.assertEqual(steps, ['tool-tests', 'inventory', 'inventory', 'test', 'test', 'record'])
        provenance = next((self.out / 'candidates').glob('*/provenance.json'))
        self.assertFalse(json.loads(provenance.read_text())['active'])

    def test_record_replaces_a_stale_prepared_candidate(self):
        # A candidate prepared before the sources changed would otherwise shadow the
        # inventory the audit records, and every later ordinary run would fail stale.
        self.prepare()
        self.source = 'source-B'
        self.assertTrue((self.out / 'prepared/provenance.json').exists())
        agent = self.base / 'target/jacoco-agent.txt'
        agent.parent.mkdir()
        agent.write_text('agent.jar')
        with patch.object(run, 'sbt'), patch.object(run, 'MODULES', ('io',)), \
             patch.object(run.subprocess, 'run'), \
             patch.object(run.subprocess, 'check_output', return_value='sha'):
            run.main(['--record'])
        self.assertFalse((self.out / 'prepared').exists())
        # A prepare-only run keeps the directory it writes.
        with patch.object(run, 'sbt'):
            run.main(['--prepare'])
        self.assertTrue((self.out / 'prepared/provenance.json').exists())

    def test_test_failure_stops_record_and_retires_candidate(self):
        agent = self.base / 'target/jacoco-agent.txt'
        agent.parent.mkdir()
        agent.write_text('agent.jar')
        def fail_test(commands, *args, **kwargs):
            if any(command.endswith('/test') for command in commands):
                candidate.select_inventory(kwargs['environment'])
                raise subprocess.CalledProcessError(1, 'coverage missing Diagnose')
        with patch.object(run, 'sbt', side_effect=fail_test), \
             patch.object(run.subprocess, 'run') as record:
            with self.assertRaises(subprocess.CalledProcessError):
                run.main(['--record'])
            record.assert_not_called()
        self.assertFalse((self.base / 'inventory-provenance.json').exists())
        provenance = next((self.out / 'candidates').glob('*/provenance.json'))
        self.assertFalse(json.loads(provenance.read_text())['active'])


class SerializationTests(unittest.TestCase):
    def test_exact_existing_wire_and_classification(self):
        rows = inventory.public_inventory([row()], [row()])
        self.assertEqual(rows['class eyes4s.plan.NewError'],
                         ['abstraction-or-container', 'plan/src/main/scala/NewError.scala',
                          'jvm+js', False, ['eyes4s.plan.Shared'], '() extends scala.reflect.Enum'])
        self.assertEqual(inventory.inventory_text(rows), '{\n  "class eyes4s.plan.NewError": '
                         '["abstraction-or-container", "plan/src/main/scala/NewError.scala", '
                         '"jvm+js", false, ["eyes4s.plan.Shared"], "() extends scala.reflect.Enum"]\n}\n')
        cases = [('method', 'foo$default$1', '', '', 'compiler-default-argument'),
                 ('value', 'equal', '', 'scala.CanEqual[X,X]', 'compile-time-equality-witness'),
                 ('value', 'item', 'Flags.ParamAccessor', '', 'structural-accessor')]
        for kind, name, flags, value, expected in cases:
            self.assertEqual(inventory.category(row(kind=kind, name=name, flags=flags,
                              valueType=value, category='runtime-entry')), expected)

    def test_platform_refusals_and_jvm_only_contract(self):
        with self.assertRaisesRegex(ValueError, 'JS-only'):
            inventory.public_inventory([], [row()])
        with self.assertRaisesRegex(ValueError, 'type difference'):
            inventory.public_inventory([row()], [row(sourceType='different')])
        with self.assertRaisesRegex(ValueError, 'JVM-only'):
            inventory.public_inventory([row()], [])
        result = inventory.public_inventory([row(source='io/.jvm/src/main/X.scala')], [])
        self.assertEqual(next(iter(result.values()))[2], 'jvm')
        self.assertEqual(inventory.public_inventory([row(category='internal')], []), {})


if __name__ == '__main__':
    unittest.main()
