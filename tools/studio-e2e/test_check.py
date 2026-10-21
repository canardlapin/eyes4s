"""A failed or incomplete run must never produce catalogue acceptance."""
import tempfile
import unittest
from pathlib import Path
from check import passed_cases

class ReportAcceptance(unittest.TestCase):
    def read(self, xml, needle="scenario"):
        with tempfile.TemporaryDirectory() as directory:
            report = Path(directory) / "report.xml"
            report.write_text(xml)
            return passed_cases(report, needle)

    def test_passed_named_case(self):
        self.assertEqual(self.read('<testsuite><testcase name="scenario works"/></testsuite>'), ["scenario works"])

    def test_failed_errored_and_skipped_cases_refuse(self):
        for tag in ("failure", "error", "skipped"):
            with self.subTest(tag=tag), self.assertRaises(ValueError):
                self.read(f'<testsuite><testcase name="scenario"><{tag}/></testcase></testsuite>')

    def test_one_failed_match_refuses_the_whole_selection(self):
        with self.assertRaises(ValueError):
            self.read('<testsuite><testcase name="scenario a"/><testcase name="scenario b"><failure/></testcase></testsuite>')

    def test_missing_named_case_refuses(self):
        with self.assertRaises(ValueError):
            self.read('<testsuite><testcase name="unrelated"/></testsuite>')

    def test_missing_report_refuses(self):
        with tempfile.TemporaryDirectory() as directory, self.assertRaises(FileNotFoundError):
            passed_cases(Path(directory) / "absent.xml", "scenario")

if __name__ == "__main__":
    unittest.main()
