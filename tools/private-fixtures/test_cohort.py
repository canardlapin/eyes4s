import unittest
from unittest.mock import patch

import end_to_end as density
from cohort import summarize
from synthetic import request


class CohortTests(unittest.TestCase):
    def test_participant_weighting_and_absent_cells(self):
        sources, scores = [], []
        for participant, cue, values in [('p1', 'a', [1, 1, 1]), ('p2', 'a', [0]),
                                          ('p1', 'b', [.25]), ('p3', 'b', [.5])]:
            for value in values:
                key = str(len(sources))
                sources.append(dict(id=key, participant=participant, phase='cue', object_type=cue, probe='target'))
                scores.append(dict(id=key, participant=participant, phase='cue',
                                   eyes4s=dict(difference=value), eyesim=dict(difference=value/2)))
        summary = summarize(dict(sources=sources), scores)
        self.assertEqual(summary['condition_means'][0]['eyes4s'], .5)
        contrast = summary['contrasts'][0]
        self.assertEqual(contrast['participants'], 1)
        self.assertEqual(contrast['eyes4s'], .75)
        self.assertEqual(contrast['unpaired_participants'], ['p2', 'p3'])
        with self.assertRaises(ValueError):
            summarize(dict(sources=sources), scores[:-1])

    @unittest.skipIf(density.np is None, 'optional NumPy not installed')
    def test_acceleration_preserves_independent_oracle(self):
        p = request()
        item = p['templates'][0]
        xs, ys = density.axes(p)
        for options in ({}, dict(quantize=True, truncate=True), dict(support_sigma=3.7)):
            fast = density.gaussian(p, item, xs, ys, **options)
            with patch.object(density, 'np', None):
                slow = density.gaussian(p, item, xs, ys, **options)
            density.assert_close(fast, slow, density.MAP_TOLERANCE, 'accelerated oracle')
        maps = {i['id']: density.gaussian(p, i, xs, ys) for i in p['templates']}
        f = density.score_function(maps)
        for a in maps:
            for b in maps:
                self.assertTrue(density.SCORE_TOLERANCE.matches(f(a,b), density.cosine(maps[a], maps[b])))
