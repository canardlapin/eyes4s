"""Select only a fresh compiler inventory; preparation is not audit qualification."""
import hashlib
import json
import os
import re
import sys
from common import ROOT, OUT, fingerprint
from inventory import public_inventory, inventory_text

RUN_ENV = 'EYES4S_API_AUDIT_RUN'
PREPARE = 'python3 tools/api-audit/run.py --prepare'


def paths(run_id=None):
    if run_id is not None:
        if not re.fullmatch('[0-9a-f]{32}', run_id):
            raise ValueError('invalid audit run identity')
        directory = OUT / 'candidates' / run_id
    else:
        directory = OUT / 'prepared'
    return directory / 'inventory.json', directory / 'provenance.json'


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def write_provenance(path, inventory, identity, mode, run_id=None):
    value = {'version': 1, 'mode': mode, 'fingerprint': identity,
             'inventory_sha256': digest(inventory)}
    if mode != 'committed':
        value['root'] = str(ROOT.resolve())
    if mode == 'audit':
        value.update(run_id=run_id, active=True)
    path.write_text(json.dumps(value, indent=2, sort_keys=True) + '\n')


def prepare_inventory(identity, run_id=None):
    if fingerprint() != identity:
        raise ValueError('sources changed during compiler inventory generation')
    rows = [json.loads((OUT / f'inventory-{axis}.json').read_text())['entries']
            for axis in ('jvm', 'js')]
    inventory, provenance = paths(run_id)
    inventory.parent.mkdir(parents=True, exist_ok=True)
    inventory.write_text(inventory_text(public_inventory(*rows)))
    write_provenance(provenance, inventory, identity,
                     'audit' if run_id is not None else 'prepared', run_id)
    return inventory


def validate(inventory, provenance, mode, run_id=None):
    value = json.loads(provenance.read_text())
    if value.get('version') != 1 or value.get('mode') != mode:
        raise ValueError('wrong inventory provenance kind or version')
    if mode != 'committed' and value.get('root') != str(ROOT.resolve()):
        raise ValueError('inventory belongs to another worktree')
    if mode == 'audit' and (value.get('run_id') != run_id or value.get('active') is not True):
        raise ValueError('inventory belongs to another or inactive audit run')
    if value.get('fingerprint') != fingerprint():
        raise ValueError('stale inventory: source/test/build fingerprint changed')
    if value.get('inventory_sha256') != digest(inventory):
        raise ValueError('inventory hash does not match provenance')
    # Reject malformed structure before handing the path to the Scala consumer.
    rows = json.loads(inventory.read_text())
    if not isinstance(rows, dict) or not rows or any(
        not isinstance(k, str) or not isinstance(v, list) or len(v) != 6
        or not isinstance(v[0], str) or not isinstance(v[1], str)
        or v[2] not in ('jvm', 'jvm+js') or not isinstance(v[3], bool)
        or not isinstance(v[4], list) or not all(isinstance(x, str) for x in v[4])
        or not isinstance(v[5], str) for k, v in rows.items()
    ):
        raise ValueError('malformed public inventory')
    return inventory


def select_inventory(environment=None):
    environment = os.environ if environment is None else environment
    run_id = environment.get(RUN_ENV)
    try:
        if run_id is not None:
            return validate(*paths(run_id), 'audit', run_id)
        inventory, provenance = paths()
        # Presence is never evidence of freshness, and a broken local candidate
        # must not be hidden by a fallback to an older committed inventory.
        if inventory.exists() or provenance.exists():
            return validate(inventory, provenance, 'prepared')
        base = ROOT / 'tools/api-audit'
        return validate(base / 'inventory.json', base / 'inventory-provenance.json', 'committed')
    except (OSError, ValueError, TypeError, AttributeError) as error:
        raise ValueError(f'API inventory unavailable: {error}. Run `{PREPARE}`; '
                         'during an audit, restart tools/api-audit/run.py instead.') from error


def retire(run_id):
    _, provenance = paths(run_id)
    if provenance.exists():
        value = json.loads(provenance.read_text())
        value['active'] = False
        provenance.write_text(json.dumps(value, indent=2, sort_keys=True) + '\n')


if __name__ == '__main__':
    try:
        print(select_inventory())
    except ValueError as error:
        sys.exit(str(error))
