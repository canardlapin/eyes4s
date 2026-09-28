"""Pinned public ASC parser conformance for the frozen synthetic inputs."""

import hashlib
from importlib.metadata import version
import json
from pathlib import Path
import tempfile
import unittest
import warnings

from pymovements.gaze.io import from_asc

import fixtures


class AscFixtureCompatibilityTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.config = json.loads(Path(__file__).with_name("performance.json").read_text())
        if version("pymovements") != cls.config["comparator"]["version"]:
            raise AssertionError("ASC compatibility must use the pinned pymovements version")

    def test_public_parser_materializes_both_eye_layouts_and_metadata(self):
        for family, binocular, rate in (("steps", False, 500),
                                        ("gaps-binocular", True, 1000)):
            with self.subTest(family=family), tempfile.TemporaryDirectory() as temporary:
                dataset = next(d for d in self.config["datasets"]
                               if d["family"] == family and d["scale"] == "small"
                               and d["format"] == "asc")
                data = b"".join(fixtures.chunks(dataset))
                self.assertEqual(hashlib.sha256(data).hexdigest(), dataset["sha256"])
                path = Path(temporary) / "fixture.asc"
                path.write_bytes(data)
                with warnings.catch_warnings(record=True) as observed:
                    warnings.simplefilter("always")
                    gaze = from_asc(path, messages=True)

                self.assertFalse(any("No tracked eye" in str(w.message) or
                                     "No screen resolution" in str(w.message)
                                     for w in observed))
                self.assertEqual(gaze.samples.height, dataset["rows"])
                self.assertEqual(gaze.messages.height, 3)
                self.assertEqual(gaze.experiment.eyetracker.sampling_rate, rate)
                self.assertIs(gaze.experiment.eyetracker.left, True)
                self.assertIs(gaze.experiment.eyetracker.right, binocular)
                self.assertEqual(gaze.experiment.screen.resolution, (1920, 1080))
                self.assertEqual(gaze.samples.row(0, named=True)["time"], 0)
                self.assertEqual(gaze.samples.row(0, named=True)["pupil"],
                                 [1000.0, 1000.0] if binocular else 1000.0)
                self.assertEqual(gaze.messages.get_column("content").to_list(), [
                    f"RECCFG CR {rate} 2 2 2 2 {'LR' if binocular else 'L'}",
                    "GAZE_COORDS 0 0 1919 1079",
                    "DISPLAY_COORDS 0 0 1919 1079",
                ])
                if binocular:
                    self.assertEqual(gaze.samples.row(450, named=True)["pixel"],
                                     [None, None, None, None])
                    self.assertEqual(gaze.samples.row(450, named=True)["pupil"],
                                     [0.0, 0.0])


if __name__ == "__main__":
    unittest.main()
