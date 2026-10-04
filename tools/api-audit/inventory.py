"""One compiler-to-reviewed-inventory projection for pre-test and final gates."""
import json

# Explicit structural classifications retain their symbols in the reviewed inventory.
def category(x):
    kind=x['category']; flags=x['flags']; owner=x['ownerFlags']
    if kind!='runtime-entry':return kind
    if '$default$' in x['name']:return 'compiler-default-argument'
    if x['signature'].endswith(':scala.CanEqual'):return 'compile-time-equality-witness'
    if x['kind']=='value':
        if 'Flags.Module' in flags:return 'module-reference'
        if 'Flags.ParamAccessor' in flags:return 'structural-accessor'
        if 'Flags.Enum' in flags:return 'enum-case-value'
        if x['valueType'].startswith('scala.CanEqual['):return 'compile-time-equality-witness'
    if x['name']=='<init>':
        if 'Flags.Module' in owner:return 'module-initializer'
        if 'Flags.Case' in owner or 'Flags.Synthetic' in owner:return 'structural-constructor'
        if 'Flags.Abstract' in owner or 'Flags.Trait' in owner:return 'abstract-constructor'
    return kind


def key(row):
    return row['kind'] + ' ' + row['id']


def public_inventory(jvm_rows, js_rows):
    public = [dict(row, classification=category(row)) for row in jvm_rows
              if category(row) not in ('internal', 'compiler-generated')]
    js = {key(row): row for row in js_rows
          if category(row) not in ('internal', 'compiler-generated')}
    jvm = {key(row): row for row in public}
    if set(js) - set(jvm):
        raise ValueError('unexpected JS-only declarations ' + repr(sorted(set(js) - set(jvm))[:8]))
    for name in set(jvm) & set(js):
        if jvm[name]['sourceType'] != js[name]['sourceType']:
            raise ValueError('unexplained platform type difference ' + name)
    only_jvm = set(jvm) - set(js)
    for name in only_jvm:
        if not jvm[name]['source'].startswith('io/.jvm/src/main/'):
            raise ValueError('unexplained JVM-only declaration ' + name)
    inventory = {
        key(row): [row['classification'], row['source'],
                   'jvm' if key(row) in only_jvm else 'jvm+js', row['deprecated'],
                   sorted(i for i in row['inherited'] if i.startswith('eyes4s.')),
                   row['sourceType']]
        for row in public
    }
    return inventory


def inventory_text(inventory):
    # Preserve the reviewed one-symbol-per-line wire format exactly.
    return '{\n' + ',\n'.join('  ' + json.dumps(k) + ': ' + json.dumps(v)
                              for k, v in sorted(inventory.items())) + '\n}\n'
