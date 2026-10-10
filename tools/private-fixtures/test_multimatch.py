import json
from pathlib import Path
import tempfile
import unittest

from multimatch import COMPONENTS, compare


class MultiMatchTests(unittest.TestCase):
    def test_failed_control_is_retained_and_prevents_reduction(self):
        def item(key,n=3):
            return dict(id=key,fixations=dict(x=list(range(n))))
        source=item('q'); source.update(participant='p',phase='Cue',template='a')
        request=dict(sources=[source],templates=[item('a'),item('b',2)],pairs=[
            dict(source='q',reference='a',kind='matched'),dict(source='q',reference='b',kind='control')])
        rows=[{**request['pairs'][0],'values':dict.fromkeys(COMPONENTS,.75)},
              {**request['pairs'][1],'error':'short path'}]
        with tempfile.TemporaryDirectory() as d:
            folder=Path(d)
            for engine in ('eyes4s','eyesim'):
                (folder/f'multimatch-{engine}.jsonl').write_text('\n'.join(json.dumps(v) for v in rows))
            queries,counts,maximum,disagreements=compare(request,folder)
            self.assertEqual(queries[0]['controls'],['b'])
            self.assertIsNone(queries[0]['eyes4s'])
            self.assertEqual(len(queries[0]['unavailable_pairs']),1)
            self.assertEqual(counts,dict(comparable=1,short_in_both=1))
            self.assertEqual(disagreements,0)
            (folder/'comparison.json').unlink()
            rows[0]['reference']='wrong'
            (folder/'multimatch-eyesim.jsonl').write_text('\n'.join(json.dumps(v) for v in rows))
            with self.assertRaises(ValueError):
                compare(request,folder)
