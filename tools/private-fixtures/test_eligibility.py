import copy
import unittest
from eligibility import select
from planned_scores import controls, ranks
from synthetic import request

class CommonEligibilityTests(unittest.TestCase):
    def test_selection_preserves_identity_and_both_operands(self):
        original=request(); p=copy.deepcopy(original)
        short=p['templates'][0]
        for values in short['fixations'].values(): del values[2:]
        result, counts=select(p)
        self.assertEqual(counts['original_queries'],len(p['sources']))
        self.assertTrue(all(len(v['fixations']['x'])>=3 for v in result['sources']+result['templates']))
        self.assertNotIn(short['id'],{s['template'] for s in result['sources']})
        self.assertEqual(p['sources'],original['sources'])
        self.assertEqual(counts['retained_queries']+sum(bool(d['reasons']) for d in result['common_eligibility']['query_decisions']),counts['original_queries'])
        self.assertTrue(all(s in p['sources'] for s in result['sources']))
    def test_frozen_draw_is_order_independent_excludes_match_and_caps(self):
        s={'id':'q','template':'a'}
        self.assertEqual(controls(s,['a','b','c','d'],42,2),controls(s,['d','a','c','b'],42,2))
        self.assertNotIn('a',controls(s,['a','b','c'],42,100))
        self.assertEqual(set(controls(s,['a','b','c'],42,100)),{'b','c'})
        self.assertEqual(len(controls(s,['a','b','c','d'],42,2)),2)
    def test_exact_ties_and_constant_failure(self):
        self.assertEqual(ranks([0,0,4,2]).tolist(),[1.5,1.5,4,3])
        with self.assertRaises(ValueError): ranks([1,1,1])

if __name__=='__main__': unittest.main()
