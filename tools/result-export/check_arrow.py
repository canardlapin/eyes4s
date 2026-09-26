#!/usr/bin/env python3
"""Independent PyArrow reader compares IPC to canonical CSV and linked schema metadata."""
import argparse,csv,json,math,hashlib
from decimal import Decimal
from pathlib import Path
import pyarrow as pa
p=argparse.ArgumentParser(description=__doc__);p.add_argument('directory',type=Path);a=p.parse_args()
def canonical(v):
 if v is None:return 'null'
 if isinstance(v,bool):return 'true' if v else 'false'
 if isinstance(v,int):return str(v)
 if isinstance(v,Decimal):
  if not v:return '0'
  s=format(v,'f')
  return s.rstrip('0').rstrip('.') if '.' in s else s
 if isinstance(v,str):return json.dumps(v,ensure_ascii=False,separators=(',',':'))
 if isinstance(v,list):return '['+','.join(map(canonical,v))+']'
 return '{'+','.join(canonical(k)+':'+canonical(v[k]) for k in sorted(v))+'}'
def pack(values):return ''.join(str(len(v.encode('utf-16-le'))//2)+':'+v for v in values)
index=json.loads((a.directory/'index.json').read_text())
frozen=json.loads((Path(__file__).parent/'schema-v1.json').read_text())
assert {r['name'] for r in index}==set(frozen)
for entry in index:
 name=entry['name'];meta=json.loads((a.directory/(name+'.metadata.json')).read_text())
 assert {k:meta[k] for k in ('schema','family','columns')}==frozen[name]
 with pa.memory_map(str(a.directory/(name+'.arrow')),'r') as source:
  table=pa.ipc.open_stream(source).read_all()
 assert table.num_rows==entry['rows']==int(meta['row_count'])
 assert json.loads(table.schema.metadata[b'eyes4s.result_metadata'])==meta
 with (a.directory/(name+'.csv')).open(newline='') as f:rows=list(csv.DictReader(f))
 assert len(rows)==table.num_rows
 exact_meta=json.loads((a.directory/(name+'.metadata.json')).read_text(),parse_float=Decimal)
 encoded_rows=[]
 for row in rows:
  encoded_rows.append(pack(['missing' if c['nullable'] and row[c['name']+'__valid']=='false' else 'present:'+row[c['name']] for c in meta['columns']]))
 content=pack([meta['schema'],meta['family'],canonical(exact_meta['context'])]+[canonical(c) for c in exact_meta['columns']]+encoded_rows)
 assert hashlib.sha256(content.encode('utf-8')).hexdigest()==meta['table_sha256'],(name,'content identity mismatch')
 expected_names=['table_sha256']+[c['name'] for c in meta['columns']]
 assert table.column_names==expected_names
 for col in meta['columns']:
  field=table.schema.field(col['name'])
  expected={'Utf8':pa.string(),'JsonUtf8':pa.string(),'Int64':pa.int64(),'Float64':pa.float64(),'Boolean':pa.bool_()}[col['type']]
  assert field.type==expected and field.nullable==col['nullable'],(name,col,field)
  assert field.metadata[b'unit'].decode()==col['unit']
  assert json.loads(field.metadata[b'labels'])==col['labels']
  assert not pa.types.is_dictionary(field.type) # Declared physical encoding, labels stay text.
 for csvrow,arrowrow in zip(rows,table.to_pylist(),strict=True):
  assert csvrow['table_sha256']==arrowrow['table_sha256']==meta['table_sha256']==entry['table_sha256']
  for col in meta['columns']:
   key=col['name'];value=arrowrow[key]
   missing=col['nullable'] and csvrow[key+'__valid']=='false'
   if missing:
    assert value is None and csvrow[key]==''
   else:
    raw=csvrow[key]
    expected={'Utf8':lambda x:x,'JsonUtf8':lambda x:x,'Int64':int,'Float64':float,'Boolean':lambda x:x=='true'}[col['type']](raw)
    assert value==expected,(name,key,value,expected)
    if isinstance(value,float):
     assert math.isfinite(value)
     if value==0:assert math.copysign(1,value)==math.copysign(1,expected)
print(f'PyArrow {pa.__version__}: every IPC value, schema, null bit, exact Int64, UTF-8 label and sidecar agrees with CSV.')
