"""Semantic checks for the pinned binocular ASC comparison adapter."""

from pathlib import Path
import sys
import tempfile
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parent))
import bench_asc


ASYMMETRIC_ASC = (
    "** Synthetic eyes4s performance fixture v2\n"
    "START 0 LEFT RIGHT SAMPLES\n"
    "PUPIL AREA\n"
    "SAMPLES GAZE LEFT\tRIGHT RATE 1000.00\n"
    "MSG 0 RECCFG CR 1000 2 2 2 2 LR\n"
    "MSG 0 GAZE_COORDS 0 0 1919 1079\n"
    "MSG 0 DISPLAY_COORDS 0 0 1919 1079\n"
    "0\t10.00\t20.00\t1000.00\t30.00\t40.00\t900.00\n"
    "1\t.\t.\t0.00\t.\t.\t0.00\n"
    "END 1 SAMPLES EVENTS RES 30.00 30.00\n"
)


class AscAdapterTests(unittest.TestCase):
    def test_both_eyes_and_gap_have_exact_canonical_rows(self):
        with tempfile.TemporaryDirectory() as temporary:
            source = Path(temporary) / "asymmetric.asc"
            output = Path(temporary) / "canonical.csv"
            source.write_text(ASYMMETRIC_ASC, encoding="utf-8")
            report = bench_asc.execute(source, output, 2)
            self.assertEqual(report["sample_rows"], 2)
            self.assertEqual(report["output_rows"], 4)
            self.assertEqual(report["messages"], 3)
            self.assertEqual(report["output_sha256"], bench_asc.sha256(output))
            self.assertEqual(output.read_text().splitlines(), [
                "time_us,eye,x_px,y_px,pupil,status",
                "0,left,10.000000,20.000000,1000.000000,tracked",
                "0,right,30.000000,40.000000,900.000000,tracked",
                "1000,left,,,,lost",
                "1000,right,,,,lost",
            ])
            with self.assertRaisesRegex(ValueError, "sample count changed"):
                bench_asc.execute(source, output, 3)


if __name__ == "__main__":
    unittest.main()
