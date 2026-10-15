"""A native R binary oracle for numerical JSON evidence at a support boundary."""
import json
from pathlib import Path
import shutil
import struct
import subprocess
import tempfile
import unittest

HERE = Path(__file__).resolve().parent


@unittest.skipUnless(shutil.which("Rscript"), "Rscript is required for the native export oracle")
class EvidenceJsonPrecisionTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        with tempfile.TemporaryDirectory(prefix="eyes4s-json-precision-") as directory:
            root = Path(directory)
            script = root / "oracle.R"
            script.write_text('''args <- commandArgs(trailingOnly = TRUE)
source(args[[1L]])
axis <- seq(-1, 7, length.out = 11)
boundary <- 3.2 - 1.8
adversarial <- c(1 + .Machine$double.eps, pi, 1e-200, 1e200)
value <- list(axis = axis, boundary = boundary, adversarial = adversarial)
writeBin(c(axis, boundary, adversarial), file.path(args[[2L]], "native.bin"),
         size = 8, endian = "big")
write_evidence_json(value, file.path(args[[2L]], "corrected.json"))
jsonlite::write_json(value, file.path(args[[2L]], "legacy.json"),
                     auto_unbox = TRUE, digits = NA)
''')
            subprocess.run(["Rscript", str(script), str(HERE / "evidence_json.R"), str(root)],
                           check=True, capture_output=True, text=True, timeout=30)
            data = (root / "native.bin").read_bytes()
            cls.native = list(struct.unpack(">" + "d" * (len(data) // 8), data))
            cls.corrected = json.loads((root / "corrected.json").read_text())
            cls.legacy = json.loads((root / "legacy.json").read_text())

    def test_axis_and_boundary_preserve_native_double_bits(self):
        decoded = self.corrected["axis"] + [self.corrected["boundary"]]
        self.assertEqual([struct.pack(">d", value) for value in decoded],
                         [struct.pack(">d", value) for value in self.native[:12]])

    def test_support_classification_survives_round_trip_and_rejects_legacy_export(self):
        native_count = sum(value <= self.native[11] for value in self.native[:11])
        corrected_count = sum(value <= self.corrected["boundary"] for value in self.corrected["axis"])
        legacy_count = sum(value <= self.legacy["boundary"] for value in self.legacy["axis"])
        self.assertEqual(corrected_count, native_count)
        self.assertNotEqual(legacy_count, native_count)

    def test_nonzero_finite_adversarial_values_preserve_native_bits(self):
        self.assertEqual([struct.pack(">d", value) for value in self.corrected["adversarial"]],
                         [struct.pack(">d", value) for value in self.native[12:]])


if __name__ == "__main__":
    unittest.main()
